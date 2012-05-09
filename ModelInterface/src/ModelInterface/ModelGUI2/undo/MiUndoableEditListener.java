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
package ModelInterface.ModelGUI2.undo;

import javax.swing.event.UndoableEditEvent;

/**
 * Create and interface to listen for undoable events. This version 
 * makes it clear as to if an undo or redo occured.
 * @author Pralit Patel
 */
public interface MiUndoableEditListener extends java.util.EventListener {
	/**
	 * Will be called when an undo occurs.  The event provides
	 * the source and the UndoableEdit that performed the undo.
	 * @param e The Event that did the undo.
	 */
	public void undoPerformed(UndoableEditEvent e);

	/**
	 * Will be called when an redo occurs.  The event provides
	 * the source and the UndoableEdit that performed the redo.
	 * @param e The Event that did the redo.
	 */
	public void redoPerformed(UndoableEditEvent e);
}
