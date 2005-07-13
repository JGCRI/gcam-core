/*
 */
package utils;

import guicomponents.DOMListModel;

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.SwingUtilities;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
/**
 * This class selectively enables and disables a set of buttons related to a list
 * based on whether the action they represent is currently possible to execute.
 *
 * @author Josh Lurz
 * 
 */
public class ButtonSetEnabler implements ListSelectionListener, ActionListener, ListDataListener {
    
    /**
     * The master list which controls the four dependent buttons.
     */
    private transient final JList mList;
    
    /**
     * A reference to the add button associated with the master list.
     */
	private transient final JButton mAdd;
    
    /**
     * A reference to the delete button associated with the master list.
     */
	private transient final JButton mDelete;
	
    /**
     * A reference to the up button associated with the master list.
     */
    private transient final JButton mUp;
	
    /**
     * A reference to the down button associated with the master list.
     */
    private transient final JButton mDown;

    
    /**
     * Constructor for the button set enabler which contains the list initializing
     * the enabling and the four buttons to enable or disable.
     * @param aList The list causing the enabling or disabling.
     * @param aAdd The add button.
     * @param aDelete The delete button.
     * @param aUp The up button.
     * @param aDown The down button.
     */
	public ButtonSetEnabler(JList aList, JButton aAdd, JButton aDelete, JButton aUp, JButton aDown){
		super();
        mList = aList;
		mAdd = aAdd;
		mDelete = aDelete;
		mUp = aUp;
		mDown = aDown;
		updateButtonStates();
	}

	/**
     * Value changed listener method which calls updateButtonStates.
     * @param aEvent The event received.
	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
	 */
	public void valueChanged(final ListSelectionEvent aEvent) {
		// Really need to do event checking.
		updateButtonStates();
	}
	
    /**
     * Method called when an action is performed on the List.
     * @param aEvent The event receieved.
     */
	public void actionPerformed(final ActionEvent aEvent){
		// Really need to event checking.
		updateButtonStates();
	}
    
    /**
     * Method called when an interval is added to the list.
     * @param aEvent The event received.
     */
    public void intervalAdded(final ListDataEvent aEvent) {
        updateButtonStates();
     }
    
     /**
      * Method called when an interval is removed from the list.
      * @param aEvent The event received.
      */
     public void intervalRemoved(final ListDataEvent aEvent) {
         updateButtonStates();
     }

     /**
      * Method called when the contents of a list change.
      * @param aEvent The event received.
      */
     public void contentsChanged(final ListDataEvent aEvent) {
         updateButtonStates();
     }
     
	/** 
     * Conditionally enable and disable the four buttons based on the state 
     * of the list.
     * 
     * Individually enables the four buttons based on whether the action linked
     * to the button is currently possible.
     * <ul>
     * <li>The add button is enabled when there is a parent in the DOM tree to add to.</li>
     * <li>The delete button is enabled when the list has a positive size and a
     * list element is selected.</li>
     * <li>The up button is enabled if the current index is greater than zero.</li>
     * <li>The down button is enabled if the index is set and not the end of the list.</li>
     * </ul>
	 */
	private void updateButtonStates() {
        // Check if this is necessary.
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // Enable the add button if there is a root node.
                mAdd.setEnabled(((DOMListModel) mList.getModel())
                        .canAddElements());

                final int listSize = mList.getModel().getSize();
                final int index = mList.getSelectedIndex();

                // Enable the delete button if the size is greater than zero and
                // an
                // element is selected.
                mDelete.setEnabled(index != -1 && listSize > 0);

                // Enable the up button if the selected element is greater than
                // 0.
                mUp.setEnabled(index > 0);

                // Enable the down button if the selected element is not the
                // last.
                mDown.setEnabled(index != -1 && index < listSize - 1);
            }
        });
    }
}
