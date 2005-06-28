/*
 * Created on Jan 9, 2005
 */
package interfaceutils;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ButtonModel;
import javax.swing.DefaultButtonModel;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import configurationeditor.ConfigurationEditor;

/**
 * A class which wraps a default button model and replaces
 * call to set and get whether the button is selected with calls that
 * query and set a value into the DOM document.
 * @author Josh Lurz
 * 
 */
public class DOMButtonModel extends DefaultButtonModel implements ButtonModel {
	/**
	* Automatically generated unique class identifier.
	 */
	private static final long serialVersionUID = -4448518797574448977L;

	/**
	 * The top level document editor.
	 */
	private ConfigurationEditor mEditor = null;
	
	/**
	 * The name of the value in the model which will store the value of this button.
	 */
	private String mValueName = null;

	/**
	 * Constructor which initializes the reference to the top level editor
	 * and the name of the configuration value.
	 * 
	 * @param aEditor
	 *            The top level document editor.
	 * @param aValueName
	 *            The name of the value in the model which stores the value of this button.
	 */
	public DOMButtonModel(ConfigurationEditor aEditor, String aValueName) {
		assert (aValueName != null);
		assert (aEditor != null);
		mEditor = aEditor;
		mValueName = aValueName;
	}

	/**
	 * Internal method to get the button model selected state from the DOM tree.
	 * 
	 * @return Whether the button is selected.
	 */
	@Override public boolean isSelected() {
		// Get the current document from the editor.
		Document document = mEditor.getDocument();

		// If there isn't a document return right away. We can't get
		// the state from a DOM that doesn't exist.
		if (document == null) {
			Logger.global.log(Level.INFO, Messages.getString("DOMButtonModel.0")); //$NON-NLS-1$
			return false;
		}
		
		// Perform the query.
		Node resultNode = Util.getResultNodeFromQuery(document, getXPath());

		// If the node is null it means that there were no results. If a value
		// does not exist this should return false as it is unset.
		if (resultNode == null) {
			Logger.global.log(Level.INFO, Messages.getString("DOMButtonModel.1") + mValueName); //$NON-NLS-1$
			return false;
		}
		// Check the text content value of the node.
		return isTextContentTrue(resultNode);
	}

	/**
	 * Internal method to set whether the button is selected. Need to make
	 * sure this is called when the underlying document changes.
	 * 
	 * @param aArmed
	 *            The new state of the button.
	 */
    @Override public void setArmed(boolean aArmed) {
		// Ignore the the setArmed when the value is false as this is the user
		// releasing the mouse.
		if (aArmed) {
			// Get the current document from the editor.
			Document document = mEditor.getDocument();

			// If there isn't a document return right away. This should not be possible.
			if (document == null) {
				Logger.global.log(Level.WARNING, Messages.getString("DOMButtonModel.2")); //$NON-NLS-1$
				return;
			}
			
			// Perform the query.
			Node resultNode = Util.getResultNodeFromQuery(document, getXPath());

			boolean previousValue = false;
			// If the node is null it means that there were no results.
			if (resultNode == null) {
				Logger.global.log(Level.INFO, Messages.getString("DOMButtonModel.3") + mValueName); //$NON-NLS-1$
				// Create a position in the DOM tree to store the value.
				resultNode = Util.addNodesForXPath(document, getXPath());
                if(resultNode == null) {
                    Logger.global.log(Level.SEVERE, Messages.getString("DOMButtonModel.4")); //$NON-NLS-1$
                }
				previousValue = false;
			}
			// Get the text content of the result node.
			else {
				previousValue = isTextContentTrue(resultNode);
			}
			// Flip the state of the button and store the value in the tree.
			String nodeValue = !previousValue ? "1" : "0"; //$NON-NLS-1$ //$NON-NLS-2$
			resultNode.setTextContent(nodeValue);
		}
		// Set the underlying button state last as this will notify any
		// listeners.
		super.setArmed(aArmed);
	}
	
	/**
	 * Helper method which checks if the text content of a node is 1 or 0.
	 * @param aNode Node of which to check the text value.
	 * @return Whether the text node child of this node is 1.
	 */
	private static boolean isTextContentTrue(Node aNode) {
		boolean result;
		// Check if the node does not have a text value.
		if (aNode.getTextContent() == null) {
			Logger.global.log(Level.WARNING, Messages.getString("DOMButtonModel.7")); //$NON-NLS-1$
			result = false;
		}
		// Otherwise check the text value. Currently the node will contain a
		// 1 or a 0, so this needs to be converted to a true false.
		else if (aNode.getTextContent().equals("1")) { //$NON-NLS-1$
			result = true;
		} else if (aNode.getTextContent().equals("0")) { //$NON-NLS-1$
			result = false;
		}
		// Check for unknown values. Warn that these exist and return false.
		else {
			Logger.global.log(Level.WARNING,
					Messages.getString("DOMButtonModel.10")); //$NON-NLS-1$
			result = false;
		}
		return result;
	}
	
	/** 
	 * Function to return the XPath for the value representing the state of this button.
	 * @return The XPath which points at the node containing the value for this button.
	 */
	private String getXPath(){
        // TODO: Fix hardcoding.
		return "/" + ConfigurationEditor.ROOT_ELEMENT_NAME + "/Bools/Value[@name='" + mValueName + "']"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
}
