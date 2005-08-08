/**
 * 
 */
package ModelInterface.ConfigurationEditor.src.configurationeditor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JPanel;

/**
 * @author Josh Lurz
 * An editor panel which implements the property change interface so 
 * it can pass along any events to Listeners that have registered with it.
 */
public abstract class AbstractEditorPanel extends JPanel implements PropertyChangeListener {
    /**
     * Listens for property change events and passes them along to any listeners
     * that are registered on this panel.
     * @param aEvent The property change event.
     * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
     */
    public void propertyChange(final PropertyChangeEvent aEvent) {
        // Deconstruct the event and redispatch it.
        firePropertyChange(aEvent.getPropertyName(), aEvent.getOldValue(), aEvent.getNewValue());
    }

}
