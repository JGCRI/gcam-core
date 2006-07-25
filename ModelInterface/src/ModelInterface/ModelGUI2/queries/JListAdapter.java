// is this where I want to put it?
package ModelInterface.ModelGUI2.queries;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionListener;

import java.util.EventListener;

/**
 * This class implements JComponentAdapter for the JList
 *
 * @author Pralit Patel
 */
public class JListAdapter implements JComponentAdapter {

	/**
	 * JList which this class is wrapping.
	 */
	private JList myComponent;

	/**
	 * Create a new adapter for the JList passed in.
	 * @param comp JList to be adpated.
	 */
	public JListAdapter(JList comp) {
		myComponent = comp;
	}
	/**
	 * Return the JComponent for which the class is wrapping.
	 * @return JComponent which the class wraps.
	 */
	public JComponent getModel() {
		return myComponent;
	}

	/**
	 * Get the current items selected.
	 * @return An integer array of indices which are selected.
	 */
	public int[] getSelectedRows() {
		return myComponent.getSelectedIndices();
	}

	/**
	 * Set the current rows selected.
	 * @param An integer array of indices which are selected.
	 */
	public void setSelectedRows(int[] selected) {
		myComponent.setSelectedIndices(selected);
	}

	/**
	 * Get the current values selected.
	 * @return An array of the values which are selected.
	 */
	public Object[] getSelectedValues() {
		return myComponent.getSelectedValues();
	}

	/**
	 * Set the selection mode the integer constant will be specific
	 * to the JComponent which is being wrapped.
	 * @param mode Int constant specifying selection mode.
	 */
	public void setSelectionMode(int mode) {
		/* List selection model should do this work for me.
		if(mode != ListSelectionModel.MULTIPLE_INTERVAL_SELECTION ||
				mode != ListSelectionModel.SINGLE_INTERVAL_SELECTION ||
				mode != ListSelectionModel.SINGLE_SELECTION) {
			// throw exception
			System.out.println("Illegal Selection mode: "+mode);
		}
		*/
		myComponent.getSelectionModel().setSelectionMode(mode);
	}

	/**
	 * Add a selection listener to the model.
	 * @param listener The listener to add.
	 */
	public void addSelectionListener(EventListener listener) {
		myComponent.getSelectionModel().addListSelectionListener((ListSelectionListener)listener);
	}

	/**
	 * Remove a selection listener from the model.
	 * @param listener The listener to remove.
	 */
	public void removeSelectionListener(EventListener listener) {
		myComponent.getSelectionModel().removeListSelectionListener((ListSelectionListener)listener);
	}
}
