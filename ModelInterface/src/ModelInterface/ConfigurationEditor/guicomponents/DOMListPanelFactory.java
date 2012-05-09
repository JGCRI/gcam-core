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
package ModelInterface.ConfigurationEditor.guicomponents;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.utils.Messages;

/**
 * A factory class which creates DOMFileListPanels which are initialized to the
 * correct parent node. The factory also tracks the the panels and resets their
 * content when the underlying document changes.
 * 
 * @author Josh Lurz
 * 
 */
public class DOMListPanelFactory implements PropertyChangeListener {
    /**
     * Reference to the top level document.
     */
    private transient Document mDocument = null;

    /**
     * A mapping of category names to the related DOMFileListPanels created by
     * this factory. This is needed to reset the lists when the underlying
     * document changes.
     * 
     */
    private final transient Map<String, DOMListPanel> mChildListPanels;

    /**
     * Constructor
     */
    public DOMListPanelFactory() {
        super();
        mChildListPanels = new HashMap<String, DOMListPanel>();
    }

    /**
     * Factory method which creates text fields with initial values and action
     * listeners to set the values back into the DOM. The factory also keeps a
     * reference to the text fields so it can reinitialize them when the
     * underlying document changes.
     * 
     * @param aCategory
     *            The location of the set of value elements for this list.
     * @param aElementName
     *            The name of the elements of the list.
     * @param aPanelLabel
     *            The label associated with this list.
     * @param aLeafChildren
     *            Whether the elements of this list are leaves. Leaf lists will
     *            add new files as new items, node lists will add elements as
     *            new items.
     * @return An initialized text field.
     */
    public DOMListPanel createDOMFileListPanel(final String aCategory,
            final String aElementName, final String aPanelLabel,
            final boolean aLeafChildren) {
        // Check if the XPath already exists in the map. There can't be multiple
        // text fields mapping to the same node in the DOM.
        if (mChildListPanels.containsKey(aCategory)) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListPanelFactory.0") + aCategory //$NON-NLS-1$
                    + Messages.getString("DOMListPanelFactory.1")); //$NON-NLS-1$
            return null;
        }

        // Create the text field and add it into the map.
        final DOMListPanel newListPanel = new DOMListPanel(aCategory, aElementName,
                aPanelLabel, aLeafChildren);
        newListPanel.setDocument(mDocument);
        mChildListPanels.put(aCategory, newListPanel);
        return newListPanel;
    }

	/**
	 * Method called when the editor which created the factory has a property change.
	 * This listener listens for the DOM document being replaced and updates it children.
	 * TODO: This list panels could listen on their own.
	 * @param aEvent The property change event.
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(final PropertyChangeEvent aEvent) {
		if (aEvent.getPropertyName().equals("document-replaced")) {
			mDocument = (Document) aEvent.getNewValue();
			// Iterate through any children text fields and reset their values.
			final Iterator<String> currPath = mChildListPanels.keySet()
					.iterator();
			while (currPath.hasNext()) {
				final DOMListPanel currList = mChildListPanels.get(currPath
						.next());
				currList.setDocument(mDocument);
			}
		}
        else {
            Logger.global.log(Level.WARNING, "Property change listener added for the wrong property change.");
        }
	}
}
