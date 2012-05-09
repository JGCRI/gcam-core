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
