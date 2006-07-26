// is this where I want to put it?
package ModelInterface.ModelGUI2.queries;

import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;

import java.util.EventListener;

/**
 * This class implements JComponentAdapter for the JTree
 *
 * @author Pralit Patel
 */
public class JTreeAdapter implements JComponentAdapter {

	/**
	 * JTree which this class is wrapping.
	 */
	private JTree myComponent;

	/**
	 * Create a new adapter for the JTree passed in.
	 * @param comp JTree to be adpated.
	 */
	public JTreeAdapter(JTree comp) {
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
		return myComponent.getSelectionRows();
	}

	/**
	 * Set the current rows selected.
	 * @param An integer array of indices which are selected.
	 */
	public void setSelectedRows(int[] selected) {
		myComponent.setSelectionRows(selected);
	}

	/**
	 * Get the current values selected.
	 * @return An array of the values which are selected.
	 */
	public Object[] getSelectedValues() {
		TreePath[] paths = myComponent.getSelectionPaths();
		Object[] ret = new Object[paths.length];
		for(int i = 0; i < paths.length; ++i) {
			ret[i] = paths[i].getLastPathComponent();
		}
		return ret;
	}

	/**
	 * Set the selection mode the integer constant will be specific
	 * to the JComponent which is being wrapped.
	 * @param mode Int constant specifying selection mode.
	 */
	public void setSelectionMode(int mode) {
		// Tree Selection Model should do the job of checking if mode is valid
		// for me
		myComponent.getSelectionModel().setSelectionMode(mode);
	}

	/**
	 * Add a selection listener to the model.
	 * @param listener The listener to add.
	 */
	public void addSelectionListener(EventListener listener) {
		myComponent.addTreeSelectionListener((TreeSelectionListener)listener);
	}

	/**
	 * Remove a selection listener from the model.
	 * @param listener The listener to remove.
	 */
	public void removeSelectionListener(EventListener listener) {
		myComponent.removeTreeSelectionListener((TreeSelectionListener)listener);
	}
}
