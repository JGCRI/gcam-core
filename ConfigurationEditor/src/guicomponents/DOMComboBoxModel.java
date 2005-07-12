/**
 * 
 */
package guicomponents;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractListModel;
import javax.swing.MutableComboBoxModel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import utils.DOMUtils;
import utils.Messages;
import utils.NodeWrapper;

/**
 * A combo box model which has as its list of values the element node children
 * of a single element node. TODO: There is a lot of code shared with the
 * DOMListModel.
 * 
 * @author Josh Lurz
 * 
 */
public class DOMComboBoxModel extends AbstractListModel implements MutableComboBoxModel {
	/**
	 * Unique identifier used for serialization.
	 */
	private static final long serialVersionUID = 4829102744109149915L;

	/**
	 * The underlying document.
	 */
	private Document mDocument = null;

	/**
	 * The root node of the combo box.
	 */

	private Node mRoot = null;

	/**
	 * The stored selected item.
	 */
	private Object mSelectedItem = null;

	/**
	 * The XPath of the parent of this combo box.
	 */
	private String mParentXPath = null;

	/**
	 * The name of the element for this combo box.
	 */
	private String mElementName = null;

	/**
	 * The name of this item.
	 */
	private String mItemName = null;

	/**
	 * Map of cached node wrappers.
	 */
	private Map<Node,Object> mCachedNodeWrappers = null;
	
	/**
	 * Constructor.
	 * 
	 * @param aDocument
	 *            The document containing the contained node of this model.
	 * @param aParentXPath
	 *            The XPath of the parent element of this node.
	 * @param aElementName
	 *            The element node name.
	 * @param aItemName
	 *            The name of this specific combo box as stored on the name
	 *            attribute.
	 */
	public DOMComboBoxModel(Document aDocument, String aParentXPath,
			String aElementName, String aItemName) {
		super();
		Logger.global.log(Level.INFO, "Creating DOM combo box model.");
		mDocument = aDocument;
		mParentXPath = aParentXPath;
		mElementName = aElementName;
		mItemName = aItemName;
		mCachedNodeWrappers = new HashMap<Node, Object>();
		// Set the root node now that all information is known.
		mRoot = getRootNode();
		
		mSelectedItem = getElementAt(0);
	}

	/**
	 * Queries the list for an element at a given position. The element at this
	 * position may not reside at the same position in the DOM due to comment
	 * nodes.
	 * 
	 * @see javax.swing.ListModel#getElementAt(int)
	 * @param aPosition
	 *            in the list to return the element at.
	 */
	public Object getElementAt(int aPosition) {
		// Convert the list index into a DOM index to determine
		// the child element to return.
		int DOMIndex = DOMUtils.getDOMIndexForListIndex(mRoot, aPosition);
		if (DOMIndex != -1) {
			Node elementNode = mRoot.getChildNodes().item(DOMIndex);
			
			// Check if the node has already had a wrapper created for it.
			Object wrapper = mCachedNodeWrappers.get(elementNode);
			
			// If the wrapper is null than one must be created for the
			// element node and cached. This is so a wrappers of the same
			// element node will always be equal.
			if(wrapper == null ){
				wrapper = NodeWrapper.createProxy(elementNode);
				mCachedNodeWrappers.put(elementNode, wrapper);
			}
			
			// Return the wrapper.
			return wrapper;
		}
		Logger.global
				.log(
						Level.WARNING,
						"getElement failed because the requested position was greater than the number of elements.");
		return null;
	}

	/**
	 * Get the size of the list.
	 * 
	 * @return The size of the list.
	 * @see javax.swing.ListModel#getSize()
	 */
	public int getSize() {
		if (!canAccessDOM()) {
			Logger.global.log(Level.INFO,
					"Returning size of zero because the DOM is inaccessable.");
			return 0;
		}
		return DOMUtils.getNumberOfElementChildren(mRoot);
	}

	/**
	 * Store the currently selected item.
	 * 
	 * @param aSelectedItem
	 *            The item selected by the user.
	 */
	public void setSelectedItem(Object aSelectedItem) {
		// Check if this is a valid item in the list but not the null
		// element used to clear selection.
		if (aSelectedItem != null
				&& DOMUtils.getDOMIndexOfObject(mRoot, aSelectedItem) == -1) {
			// Specification says to do nothing in this case.
			return;
		}

		// Check if the item is already selected to avoid firing a 
		// contents changed event.
		if(aSelectedItem == mSelectedItem){
			return;
		}
		mSelectedItem = aSelectedItem;

		// Call the default list models set selected item
		// to notify listeners. Set the range of items that have
		// changed to (-1,-1) to signal that the selection has changed
		// and not the contents.
		fireContentsChanged(this, -1, -1);
	}

	/**
	 * Get the currently selected item.
	 * 
	 * @return The stored selected item.
	 */
	public Object getSelectedItem() {
		return mSelectedItem;
	}

	/**
	 * Returns whether the DOM is currently accessable.
	 * 
	 * @return Whether the DOM is accessable.
	 */
	boolean canAccessDOM() {
		return (mDocument != null && mRoot != null);
	}

	/**
	 * Return the root of this combo box in the DOM tree.
	 * 
	 * @return The root node.
	 */
	Node getRootNode() {
		Node rootNode = DOMUtils.getResultNodeFromQuery(mDocument, getXPath());
		// Create the root node if there is not one present in the DOM.
		if (rootNode == null) {
			Logger.global.log(Level.INFO,
					"Creating new root node for combo box model with XPath: "
							+ getXPath());
			rootNode = DOMUtils.addNodesForXPath(mDocument, getXPath());
			// Check for failure adding nodes to the tree.
			if (rootNode == null) {
				Logger.global.log(Level.WARNING,
						"Failed to create a root node for combo box.");
			}
		}
		return rootNode;
	}

	/**
	 * Function to return the XPath for the root node containing the information
	 * for this combobox.
	 * 
	 * @return The XPath string which will locate the node which contains the
	 *         information for this combo box model.
	 */
	private String getXPath() {
		String XPath = "";
		if (mParentXPath != null) {
			XPath += mParentXPath;
		}
		XPath += "/" + mElementName;
		if (mItemName != null) {
			XPath += "[@name='" + mItemName + "']"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return XPath;
	}

	/**
	 * Add an element to the combo box.
	 * 
	 * @param aObject
	 *            The object to add.
	 */
	public void addElement(Object aObject) {
		// Call insert element at with a position one past the end
		// to cause the element to be added at the end.
		insertElementAt(aObject, getSize());
	}

	/**
	 * Remove a list element from the list.
	 * 
	 * @param aObject
	 *            The list object to remove.
	 */
	public void removeElement(Object aObject) {
		// Find the list index of the object.
		int listIndex = DOMUtils.getListIndexOfObject(mRoot, aObject);

		// Check if the item could not be found.
		if (listIndex == -1) {
			Logger.global.log(Level.WARNING,
					"Could not find the object to remove from the combo box.");
			return;
		}
		// Allow removeElementAt to do the rest of the work
		// including notifying the listeners.
		removeElementAt(listIndex);
	}

	/**
	 * Insert an element at a specific position.
	 * 
	 * @param aObject
	 *            Object to insert.
	 * @param aPosition
	 *            List position to insert the object.
	 */
	public void insertElementAt(Object aObject, int aPosition) {
		// Create a new element and add a name attribute which is the
		// value of the object. This has to be done before the uniqueness
		// check so that searching for the item is easier.
		Element newElement = DOMUtils.createElement(mRoot, mElementName,
				aObject, false);

		// Check if the element is unique. This will fail if the createElement
		// function failed and returned a null element.
		if (DOMUtils.getDOMIndexOfObject(mRoot, newElement) != -1) {
			// This interface limits our ability to return errors. The caller
			// should have checked uniqueness before requesting the add
			// operation.
			Logger.global.log(Level.WARNING, Messages
					.getString("DOMListModel.9")); //$NON-NLS-1$
			return;
		}

		// Find the node to insert before.
		int positionAfter = DOMUtils.getDOMIndexForListIndex(mRoot, aPosition);
		Node nextNode = mRoot.getChildNodes().item(positionAfter);

		// Add the new child to the list.
		mRoot.insertBefore(newElement, nextNode);
		
		// Check if this was the first element added to the list.
		if(getSize() == 1){
			setSelectedItem(getElementAt(0));
		}
		// Notify that the list changed, which is required for GUI updates.
		// Is this right?
		fireIntervalAdded(this, aPosition, aPosition);
	}

	/**
	 * Remove a list element at a specified position.
	 * 
	 * @param aPosition
	 *            The list position to remove the element at.
	 */
	public void removeElementAt(int aPosition) {
		// Find the index within the DOM of the list item.
		int domIndex = DOMUtils.getDOMIndexForListIndex(mRoot, aPosition);

		// Check if the item could not be found.
		if (domIndex == -1) {
			Logger.global.log(Level.WARNING,
					"Could not remove element because position was invalid.");
			return;
		}

		// Check if the item removed was the selected item.
		if(getElementAt(aPosition) == mSelectedItem){
			// Get the element before position or the first element
			// if the removed position is zero. If the first position
			// is empty, this will set the selected item to null.
			mSelectedItem = getElementAt(aPosition == 0 ? 1 : aPosition - 1 );
		}
		// Remove the object.
		mRoot.removeChild(mRoot.getChildNodes().item(domIndex));

		// Alert the list that part of it changed. Uses list indexes,
		// not DOM indexes.
		fireIntervalRemoved(this, aPosition, aPosition);
	}
}
