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
package ModelInterface.ConfigurationEditor.guihelpers;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;

/**
 * This property change listener watches for a document modified event and add
 * the needs-save attribute to the document.
 * 
 * @author Josh Lurz
 */
final public class DOMDocumentSaveSetter implements PropertyChangeListener {
    /**
     * The source document.
     */
    final transient Document mDocument;

    /**
     * Constructor
     * 
     * @param aDocument
     *            The document to which this document is attached.
     */
    public DOMDocumentSaveSetter(final Document aDocument) {
        super();
        mDocument = aDocument;
    }

    /**
     * Method called when the document-modified property change is fired. This
     * property change listener should be hooked to that property change only.
     * 
     * @param aEvent
     *            The mutation event received.
     */
    public void propertyChange(final PropertyChangeEvent aEvent) {
        if (aEvent.getPropertyName().equals("document-modified")) {
            // Avoid recursively setting events.
            if (!mDocument.getDocumentElement().getAttribute("needs-save")
                    .equals("true")) {
                mDocument.getDocumentElement().setAttribute(
                        "needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        } else {
            Logger.global
                    .log(Level.WARNING,
                            "Document save setter listeners was hooked up to the wrong property change.");

        }
    }
}
