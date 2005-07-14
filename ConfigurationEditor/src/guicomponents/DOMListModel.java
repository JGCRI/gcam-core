/*
 */
package guicomponents;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractListModel;
import javax.swing.JList;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.w3c.dom.*;

import utils.DOMUtils;
import utils.Messages;
import utils.NodeWrapper;

/**
 * The DOMListModel is an implementation of a list which has content dependent
 * on an underlying DOM tree.
 * 
 * @author Josh Lurz
 */
public class DOMListModel extends AbstractListModel implements
		ListSelectionListener, ListDataListener {
	/**
	 * Automatically generated unique class identifier.
	 */
	private static final long serialVersionUID = 5114598661300941143L;

	/**
	 * The root node of the list, the children of which are displayed as list
	 * items.
	 */
	private transient Node mRoot = null;

	/**
	 * The document the list is contained in.
	 */
	private transient Document mDocument = null;

	/**
	 * The name of the elements which are considered list items.
	 */
	private final transient String mElementName;

	/**
	 * A string containing the XPath to the parent node.
	 */
	private transient String mParentXPath = null;

	/**
	 * The name of the element containing the list. TODO: Is this needed?
	 */
	private final transient String mContainerName;

	/**
	 * Whether the children of this list are leaves of the DOM tree, if not they
	 * have children with child nodes. This determines whether the text content
	 * of nodes are set to the value of the list item.
	 */
	private final transient boolean mLeafChildren;

	/**
	 * A list which this list is the child of. TODO: More docs here.
	 */
	private transient JList mParentList = null;

	/**
	 * A list which this model is containing the data for.
	 */
	private final transient JList mContainerList;

	/**
	 * Constructor which sets the child element names.
	 * 
	 * @param aContainerList
	 *            The list for which this model is containing information.
	 * @param aElementName
	 *            The name of the elements which are considered list items.
	 * @param aContainerName
	 *            The name of the container element for the list.
	 * @param aLeafChildren
	 *            Whether the elements of this list are leaves, if false they
	 *            have node children. This determines whether the text content
	 *            of the nodes should be set.
	 */
	public DOMListModel(JList aContainerList, String aElementName,
			String aContainerName, boolean aLeafChildren) {
        super();
		// TODO: FIX and comment.
		mContainerList = aContainerList;
		mContainerName = aContainerName;
		mParentXPath = mContainerName;
		mElementName = aElementName;
		mLeafChildren = aLeafChildren;
	}

	/**
	 * Get the parent list.
	 * TODO: This is bad.
	 * @return The parent list.
	 */
	public JList getParentList(){
		return mContainerList;
	}
	
	/**
	 * Set the parent list of the list.
	 * 
	 * @param aParentList
	 *            The parent list.
	 */
	public void setParentList(final JList aParentList) {
		mParentList = aParentList;
	}

	/**
	 * Set a new document for the list.
	 * 
	 * @param aDocument
	 *            The new document.
	 */
	public void setDocument(final Document aDocument) {
		mDocument = aDocument;
		mRoot = null;

		// If the parent XPath starts with / this is a root level node
		// and can set its content now.
		if (mParentXPath.charAt(0) == '/' && (mDocument != null)) { //$NON-NLS-1$
			// Set the value for the field. Query the DOM for a value
			// for this XPath. If there is not a value in the DOM the list
			// will be initialized to empty.
			Node result = DOMUtils.getResultNodeFromQuery(mDocument,
					mParentXPath);

			// Create the node for the list if there isn't one already.
			if (result == null) {
				result = DOMUtils.addNodesForXPath(mDocument,
						mParentXPath);
			}
			mRoot = result;
		}
		// Does this need to be on the Swing thread?
		fireContentsChanged(this, 0, getSize() - 1);
	}

	/**
	 * Get the size of the list.
	 * 
	 * @return The size of the list.
	 * @see javax.swing.ListModel#getSize()
	 */
	public int getSize() {
		// This can get called before the document is initialized
		// so check here to avoid the warning that the utility
		// function would print.
		if (!canAccessDOM()) {
			return 0;
		}
		return DOMUtils.getNumberOfElementChildren(mRoot);
	}

	/**
	 * Determines if a given element exists in the list by searching the DOM.
	 * 
	 * @param aMatchObject
	 *            An object for which to search the DOM.
	 * @return Whether the item exists in the DOM.
	 */
	public boolean contains(final Object aMatchObject) {
		// Create a new element and add a name attribute which is the
		// value of the object. This has to be done before the uniqueness
		// check so that searching for the item is easier. This will
		// perform error checking on the match object and the root.
		final Element newElement = DOMUtils.createElement(mRoot, mElementName,
				aMatchObject, mLeafChildren);

		// Search the DOM for the item.
		return (DOMUtils.getDOMIndexOfObject(mRoot, newElement) != -1);
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
	public Object getElementAt(final int aPosition) {
		// Convert the list index into a DOM index to determine
		// the child element to return.
		final int DOMIndex = DOMUtils.getDOMIndexForListIndex(mRoot, aPosition);
		if (DOMIndex != -1) {
			return NodeWrapper
					.createProxy(mRoot.getChildNodes().item(DOMIndex));
		}

		// Report that the position does not exist.
		Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.5")); //$NON-NLS-1$
		return null;
	}

	/**
	 * Add a single list item to the list. This operation adds the objects
	 * string value to the list as the name attribute of a new node in the DOM.
	 * If this list has children which are leaves this will also set the text
	 * content of the new node to the same value. This will only succeed if the
	 * new item does not have the same value as an existing item in the list.
	 * TODO: Figure out how to signal an error.
	 * 
	 * @param aObject
	 *            New list item to add to the list.
	 */
	public void addElement(final Object aObject) {
		// Create a new element and add a name attribute which is the
		// value of the object. This has to be done before the uniqueness
		// check so that searching for the item is easier.
		final Element newElement = DOMUtils.createElement(mRoot, mElementName,
				aObject, mLeafChildren);
		// Check if the name is unique. This will fail if the createElement
		// function failed and returned a null element.
		if (DOMUtils.getDOMIndexOfObject(mRoot, newElement) != -1) {
			// This interface limits our ability to return errors. The caller
			// should have checked uniqueness before requesting the add
			// operation.
			Logger.global.log(Level.WARNING, Messages
					.getString("DOMListModel.9")); //$NON-NLS-1$
			return;
		}

		// Add the new child to the list.
		mRoot.appendChild(newElement);

		// Notify that the list changed, which is required for GUI updates.
		final int index = DOMUtils.getListIndexOfObject(mRoot,
				newElement);
		fireIntervalAdded(this, index, index);
	}

	/**
	 * Return whether this list is ready to have elements added to it.
	 * 
	 * @return Whether the list can have items added onto it.
	 */
	public boolean canAddElements() {
		return canAccessDOM();
	}

	/**
	 * Remove an element from the list.
	 * 
	 * @param aObject
	 *            Object to remove from the list.
	 * @return Whether the item was successfully removed.
	 */
	public boolean removeElement(final Object aObject) {
		// Find the index within the DOM of the item. This function
		// will check for a null object.
		final int domIndex = DOMUtils.getDOMIndexOfObject(mRoot, aObject);

		// TODO: Evaluate if better return values could be found for errors.
		// Check if the item could not be found.
		if (domIndex == -1) {
			Logger.global
					.log(
							Level.WARNING,
							Messages.getString("DOMListModel.11") + aObject.toString() + Messages.getString("DOMListModel.12")); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		}

		// Remove the object.
		final int removedListIndex = DOMUtils.getListIndexOfObject(mRoot, aObject);
		mRoot.removeChild(mRoot.getChildNodes().item(domIndex));

		// Alert the list that part of it changed. Uses list indexes,
		// not DOM indexes.
		fireIntervalRemoved(this, removedListIndex, removedListIndex);
		return true;
	}

	/**
	 * Move an element back a single position in the list.
	 * 
	 * @param aObject
	 *            Element to move back in the list.
	 * @return The new index of the element.
	 */
	public int moveElementBack(final Object aObject) {
		// Find the index within the DOM of the item.
		final int domIndex = DOMUtils.getDOMIndexOfObject(mRoot, aObject);

		// Check if the item could not be found.
		// TODO: Evaluate if better return values could be found for errors.
		if (domIndex == -1) {
			Logger.global
					.log(
							Level.WARNING,
							Messages.getString("DOMListModel.14") + aObject.toString() + Messages.getString("DOMListModel.15")); //$NON-NLS-1$ //$NON-NLS-2$
			return 0;
		}

		// Check if its the first item in the list, which cannot be moved back.
		if (domIndex == 0) {
			Logger.global.log(Level.WARNING, Messages
					.getString("DOMListModel.16")); //$NON-NLS-1$
			return 0;
		}

		// Move the item back.
		final NodeList children = mRoot.getChildNodes();
		Node curr = children.item(domIndex);
		final Node prev = DOMUtils.getItemBefore(mRoot, curr);
		curr = mRoot.removeChild(curr);
		mRoot.insertBefore(curr, prev);

		// Fire an event off that the range between the old position of the item
		// and the new position of the item changed.
		fireContentsChanged(this, DOMUtils.getListIndexOfObject(mRoot, curr),
				DOMUtils.getListIndexOfObject(mRoot, prev));

		// Return the new location of the item so it is still highlighted.
		// Determine the new list index of the moved item. This is
		// not always the DOM index.
		return DOMUtils.getListIndexOfObject(mRoot, curr);
	}

	/**
	 * Move an element up a single position in the list.
	 * 
	 * @param aObject
	 *            Element to move up in the list.
	 * @return The new index of the element.
	 */
	public int moveElementForward(final Object aObject) {
		// Find the index within the DOM of the item.
		final int domIndex = DOMUtils.getDOMIndexOfObject(mRoot, aObject);

		// Check if the item could not be found.
		if (domIndex == -1) {
			Logger.global
					.log(
							Level.WARNING,
							Messages.getString("DOMListModel.18") + aObject.toString() + Messages.getString("DOMListModel.19")); //$NON-NLS-1$ //$NON-NLS-2$
			return 0;
		}

		// Check if its the last item in the list, which cannot be moved up.
		if (domIndex == getSize() - 1) {
			Logger.global.log(Level.WARNING, Messages
					.getString("DOMListModel.0")); //$NON-NLS-1$
			return 0;
		}
		final NodeList children = mRoot.getChildNodes();
		Node curr = children.item(domIndex);
		// Find a node to insert this item before. If its the second to
		// last item, the item to insert before is null because there
		// won't be an item after it.
		final Node prev = DOMUtils.getItemAfter(mRoot, DOMUtils.getItemAfter(mRoot,
				curr));
		curr = mRoot.removeChild(curr);
		mRoot.insertBefore(curr, prev);
		fireContentsChanged(this, DOMUtils.getListIndexOfObject(mRoot, DOMUtils
				.getItemBefore(mRoot, curr)), DOMUtils.getListIndexOfObject(
				mRoot, prev));

		// Return the new location of the item so it is still highlighted.
		// Determine the new list index of the moved item. This is
		// not related to the DOM index.
		return DOMUtils.getListIndexOfObject(mRoot, curr);
	}

	/**
	 * An internal method which determines whether the current state of the
	 * object allows methods which access the internal DOM tree to be called
	 * 
	 * @return Whether access to internal data is currently allowed.
	 */
	private boolean canAccessDOM() {
		return (mRoot != null);
	}

	/**
	 * Method to return the XPath of the currently selected item.
	 * @param aElementIndex The child node index to create an XPath for.
	 * @return The XPath for the selected item.
	 * TODO: I think this is wrong, is aElementIndex a DOM index.
	 */
	public String getSelectedXPath(final int aElementIndex) {
		return mParentXPath + "/" + mElementName + "[@name='" //$NON-NLS-1$ //$NON-NLS-2$
				+ DOMUtils.getNameAttrValue((Node) getElementAt(aElementIndex))
				+ "']"; //$NON-NLS-1$
	}

	/**
	 * Method called when the selection in the parent list is changed.
	 * 
	 * @param aEvent
	 *            The even received.
	 */
	public void valueChanged(final ListSelectionEvent aEvent) {
		mContainerList.setSelectedIndex(-1);
		update(((JList) aEvent.getSource()).getSelectedIndex());
	}

	/**
	 * Method called when an interval in the parent list is added.
	 * 
	 * @param aEvent
	 *            The event received.
	 */
	public void intervalAdded(final ListDataEvent aEvent) {
		// update(-1);
	}

	/**
	 * Method called when an interval is removed from the parent list.
	 * 
	 * @param aEvent
	 *            aEvent The event received.
	 */
	public void intervalRemoved(final ListDataEvent aEvent) {
		// update(-1);
	}

	/**
	 * Method called when the contents of the parent list is changed.
	 * 
	 * @param aEvent
	 *            The event received.
	 */
	public void contentsChanged(final ListDataEvent aEvent) {
		mContainerList.setSelectedIndex(-1);
		update(((DOMListModel) aEvent.getSource()).mContainerList
				.getSelectedIndex());
	}

	/**
	 * Internal method which updates the list based on an event received from a
	 * parent list.
	 * 
	 * @param aNewIndex
	 *            The index now selected.
	 */
	private void update(final int aNewIndex) {
		mParentXPath = ((DOMListModel) mParentList.getModel())
				.getSelectedXPath(aNewIndex);

		// Set the value for the field. Query the DOM for a value
		// for this XPath. If there is not a value in the DOM the list
		// will be initialized to empty.
		Node result = DOMUtils.getResultNodeFromQuery(mDocument,
				mParentXPath);

		// Create the node for the list if there isn't one already.
		if (result == null) {
			result = DOMUtils.addNodesForXPath(mDocument,
					mParentXPath);
		}
		mRoot = result;
		fireContentsChanged(this, 0, getSize() - 1);
	}
}
