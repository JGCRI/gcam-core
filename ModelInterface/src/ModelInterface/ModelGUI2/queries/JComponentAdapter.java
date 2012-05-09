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

import java.util.EventListener;

/**
 * Classes which implement this adapter will be able to be plugged
 * into the create query wizard as the method for selecting options.
 *
 * @author Pralit Patel
 */
public interface JComponentAdapter {
	/**
	 * Return the JComponent for which the class is wrapping.
	 * @return JComponent which the class wraps.
	 */
	public JComponent getModel();

	/**
	 * Get the current items selected.
	 * @return An integer array of indices which are selected.
	 */
	public int[] getSelectedRows();

	/**
	 * Set the current rows selected.
	 * @param An integer array of indices which are selected.
	 */
	public void setSelectedRows(int[] selected);

	/**
	 * Get the current values selected.
	 * @return An array of the values which are selected.
	 */
	public Object[] getSelectedValues();

	/**
	 * Set the selection mode the integer constant will be specific
	 * to the JComponent which is being wrapped.
	 * @param mode Int constant specifying selection mode.
	 */
	public void setSelectionMode(int mode);

	/**
	 * Add a selection listener to the model.
	 * @param listener The listener to add.
	 */
	public void addSelectionListener(EventListener listener);

	/**
	 * Remove a selection listener from the model.
	 * @param listener The listener to remove.
	 */
	public void removeSelectionListener(EventListener listener);
}
