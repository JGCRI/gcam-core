package guicomponents;


import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JTextField;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import utils.DOMUtils;
import utils.Messages;

import configurationeditor.ConfigurationEditor;

/**
 * A factory class which creates JTextFields which are setup with
 * their initial value from the DOM and have an ActionListener which
 * will add their final value back to the DOM after it is set. Another
 * option would be to do this with the Document underlying the text field.
 * @author Josh Lurz
 *
 */
public class DOMTextFieldFactory implements PropertyChangeListener {
	/**
	 * Reference to the top level document.
	 */
	private transient Document mDocument;
	
	/**
	 * A mapping of XPaths to the related JTextFields created by this factory.
	 * This is needed to reset the values of the field
	 * when the Document changes.
	 *
	 */
	private transient Map<String,JTextField> mChildTextFields = null;
	
	/**
	 * Constructor
	 */
	public DOMTextFieldFactory() {
        super();
		mChildTextFields = new HashMap<String,JTextField>();
	}
	
	/**
	 * Factory method which creates text fields with initial values and
	 * action listeners to set the values back into the DOM. The factory
	 * also keeps a reference to the text fields so it can reinitialize them
	 * when the underlying document changes.
	 * @param aCategory The location of the Value element for this item.
	 * @param aValueName The name of the Value where the text field will be stored.
	 * @return An initialized text field.
	 */
	public JTextField createTextField(final String aCategory, final String aValueName){
		// Create the xpath for this text field.
        final String xPath = createXPath(aCategory, aValueName);
		// Check if the XPath already exists in the map. There can't be multiple
		// text fields mapping to the same node in the DOM.
		if(mChildTextFields.containsKey(xPath)){
			Logger.global.log(Level.WARNING, Messages.getString("DOMTextFieldFactory.2") + aValueName //$NON-NLS-1$
					+ Messages.getString("DOMTextFieldFactory.3")); //$NON-NLS-1$
			return null;
		}
		
		// Create the text field and add it into the map.
        final JTextField newField = new JTextField();
		mChildTextFields.put(xPath, newField);
		
		// Set an initial value for the field. Query the DOM for a value
		// for this XPath. If there is not a value in the DOM the field
		// will be initialized to empty.
        final Node result = DOMUtils.getResultNodeFromQuery(mDocument, xPath);
		if(result != null && result.getTextContent() != null){
			newField.setText( result.getTextContent() );
		}
		
		// Add an action listener to the field which will update the field
		// when the user inserts a value.
		newField.addFocusListener(new DOMTextFieldFocusListener(xPath));
		return newField;
	}
	
	/**
	 * Helper method to create an XPath from the category and value.
	 * @param aCategory The location of the Value element for this item.
	 * @param aValueName The name of the Value where the text field will be stored.
	 * @return The XPath to this item.
	 */
	private static String createXPath(final String aCategory, final String aValueName){
		return "/" + ConfigurationEditor.ROOT_ELEMENT_NAME 
                   + "/" + aCategory +"/Value[@name='" + aValueName + "']"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                   
	}
    
    /**
     * An internal focus listener class which updates the node in the
     * DOM corresponding to this text field when the user removes 
     * focus from the text field. The DOM is only updated when the value
     * is changed to limit DOM mutation events.
     * @author Josh Lurz
     */
    private final class DOMTextFieldFocusListener implements FocusListener {
        /**
         * The XPath for the text field the focus listener is attached to.
         */
        private transient final String mXPath;
        
        /**
         * Constructor which sets the XPath.
         * @param aXPath The XPath for the text field the focus listener is attached to.
         */
        public DOMTextFieldFocusListener(final String aXPath){
            super();
            mXPath = aXPath;
        }
        
        /**
         * Method called when focus is lost on the text field. This 
         * method will save the value into the DOM tree.
         * @param aEvent The focus event.
         */
        public void focusLost(final FocusEvent aEvent){
            // When focus is received save the item to the current document. This
            // chunk of code could use a utility method.
            // If there isn't a document return right away. This should not be possible.
            if (mDocument == null) {
                Logger.global.log(Level.WARNING, Messages.getString("DOMTextFieldFactory.0")); //$NON-NLS-1$
                return;
            }
            
            // Perform the query.
            Node resultNode = DOMUtils.getResultNodeFromQuery(mDocument, mXPath);
            final String newContent = ((JTextField)aEvent.getSource()).getText();
            // If the node is null it means that there were no results. Don't create a new
            // node if the value which will be set in is blank. This avoid adding unnecessary
            // nodes to the DOM.
            if (resultNode == null && newContent.length() > 0) {
                Logger.global.log(Level.INFO, Messages.getString("DOMTextFieldFactory.1") + mXPath); //$NON-NLS-1$
                // Create a position in the DOM tree to store the value.
                resultNode = DOMUtils.addNodesForXPath( mDocument, mXPath);
            }
            // Check if the text was unchanged. This prevents the DOM from
            // being modified unneccessarily.
            if((resultNode != null) && (!resultNode.getTextContent().equals(newContent))){
                // Store the text field's value into the node.
                resultNode.setTextContent(newContent);
            }
        }
        
        /**
         * Method called when focus is gained. This event does not need 
         * to be acted on.
         * @param aEvent The focus event.
         */
        public void focusGained(final FocusEvent aEvent){
            // Don't do anything when focus is received.
        }
    }

	/**
	 * Method called when a bound property of the object this factory is listening on
	 * is changed. This will listen for the document-changed event which is called when
	 * the source document of an editor is changed. It will replace the factory's internal
	 * document and update all created elements.
	 * @param aEvent The property changed event received.
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(final PropertyChangeEvent aEvent) {
		if (aEvent.getPropertyName().equals("document-replaced")) {
			mDocument = (Document) aEvent.getNewValue();
			// Iterate through any children text fields and reset their values.
			final Iterator<String> currPath = mChildTextFields.keySet()
					.iterator();
			while (currPath.hasNext()) {
				final String xPath = currPath.next();
				// Query the DOM for a value for this XPath.
				final Node result = DOMUtils.getResultNodeFromQuery(mDocument,
						xPath);
				String newValue = null;
				if (result != null && result.getTextContent() != null) {
					newValue = result.getTextContent();
				}
				// Set the value into the current field. If there was not a
				// value returned from the DOM this will set null into the field
				// which needs to occur to erase an old value.
				final JTextField currField = mChildTextFields.get(xPath);
				currField.setText(newValue);
			}
		}
	}
}
