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
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Listener which enables a component when the document it is listening
 * on is set to a non-null value.
 * @author Josh Lurz
 *
 */
public final class ComponentEnabler implements PropertyChangeListener {
	/**
	 * Component to enable or disable.
	 */
	private transient final Component mComponent;

	/**
	 * Constructor
	 * @param aComponent The component to enable or disable.
	 */
	public ComponentEnabler(Component aComponent) {
		super();
		mComponent = aComponent;
	}

	/**
	 * Method called when a property change is received which checks if
	 * it is a document replaced event and enables the component if the new
	 * document is not null.
	 * @param aEvent The property change event received.
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(final PropertyChangeEvent aEvent) {
		// Check if it is a document replaced event.
		if (aEvent.getPropertyName().equals("document-replaced")) {
			// Enable if the new value which is the new document is
			// non-null.
			mComponent.setEnabled(aEvent.getNewValue() != null);
		}
        else {
            Logger.global.log(Level.WARNING, "Property change listener registered for the wrong property change.");
        }
	}
}
