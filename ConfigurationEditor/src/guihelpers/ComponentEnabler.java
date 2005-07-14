package guihelpers;

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

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
	}
}