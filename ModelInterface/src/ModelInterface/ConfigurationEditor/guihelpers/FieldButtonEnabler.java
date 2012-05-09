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

import javax.swing.JButton;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * A listener which enables a button when text is added to a field.
 * 
 * @author Josh Lurz
 */
public final class FieldButtonEnabler implements DocumentListener {
    /**
     * A button to enable when text is added to the field.
     */
    private final transient JButton mButton;

    /**
     * Constructor
     * 
     * @param aButton
     *            Button to enable when text is added to the field.
     */
    public FieldButtonEnabler(final JButton aButton) {
        super();
        mButton = aButton;
    }

    /**
     * Insert update event received from the document.
     * 
     * @param aEvent
     *            The document event.
     * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.DocumentEvent)
     */
    public void insertUpdate(final DocumentEvent aEvent) {
        enable(aEvent);
    }

    /**
     * Remove update event received from the document.
     * 
     * @param aEvent
     *            The document event.
     * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.DocumentEvent)
     */
    public void removeUpdate(final DocumentEvent aEvent) {
        enable(aEvent);
    }

    /**
     * Changed update event received from the document.
     * 
     * @param aEvent
     *            The document event.
     * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.DocumentEvent)
     */
    public void changedUpdate(final DocumentEvent aEvent) {
        enable(aEvent);
    }

    /**
     * Enable the button based on the state of the text field.
     * 
     * @param aEvent
     *            Document event received.
     */
    private void enable(final DocumentEvent aEvent) {
        // Activate the edit button if there is text in the field.
        mButton.setEnabled(aEvent.getDocument().getLength() > 0);
    }
}
