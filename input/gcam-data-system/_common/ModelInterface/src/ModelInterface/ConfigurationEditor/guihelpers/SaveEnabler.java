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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * Listener which enables a save component based on state changes in the
 * underlying document.
 * 
 * @author Josh Lurz
 * 
 */
public final class SaveEnabler implements PropertyChangeListener {
	/**
	 * Component to disable or enable.
	 */
	private final transient Component mComponent;

	/**
	 * Constructor
	 * 
	 * @param aComponent
	 *            Component to disable or enable.
	 */
	public SaveEnabler(Component aComponent) {
		super();
		mComponent = aComponent;
	}

	/**
	 * Method called when a property change is received from the editor. The
	 * method checks if this is a document modified event, document replaced
	 * event, or a document saved event. If it is a document modified event it
	 * enables the component if it was not already enabled. If it is a
	 * document-replaced event it disables the component. Finally, if it is a
	 * document saved event, it disables the save button.
	 * @param aEvent The event received.
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(final PropertyChangeEvent aEvent) {
		// Check if this is the document modified event.
        System.out.println("PROPERTY CHANGE" + aEvent.getPropertyName());
		if (aEvent.getPropertyName().equals("document-modified")) {
            System.out.println("SETTING DOCUMENT MODIFED + " + aEvent.getOldValue() + " " + aEvent.getNewValue());
			// Enable the button if it was not currently enabled and
			// should be.
			if (aEvent.getNewValue().equals(Boolean.TRUE)) {
				mComponent.setEnabled(true);
			}
		} else if (aEvent.getPropertyName().equals("document-replaced")) {
            System.out.println("DOCUMENT REPLACED");
			// Disable the component as a new document was created or an old one
			// was loaded.
			mComponent.setEnabled(false);
		} else if (aEvent.getPropertyName().equals("document-saved")) {
            System.out.println("DOCUMENT SAVEd");
			// Disable the component as a save just occurred.
			mComponent.setEnabled(false);
		}
	}
}
