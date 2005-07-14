package guihelpers;

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
		if (aEvent.getPropertyName().equals("document-modified")) {
			// Enable the button if it was not currently enabled and
			// should be.
			if (aEvent.getOldValue().equals(Boolean.FALSE)
					&& aEvent.getNewValue().equals(Boolean.TRUE)) {
				mComponent.setEnabled(true);
			}
		} else if (aEvent.getPropertyName().equals("document-replaced")) {
			// Disable the component as a new document was created or an old one
			// was loaded.
			mComponent.setEnabled(false);
		} else if (aEvent.getPropertyName().equals("document-saved")) {
			// Disable the component as a save just occurred.
			mComponent.setEnabled(false);
		}
	}
}