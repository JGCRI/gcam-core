package guihelpers;

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