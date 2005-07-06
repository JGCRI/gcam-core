/*
 */
package interfaceutils;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.w3c.dom.*;
/**
 * The DOMListModel is an implementation of a list which has 
 * content dependent on an underlying DOM tree.
 * @author Josh Lurz
 */
public class DOMListModel extends DefaultListModel implements ListSelectionListener, ListDataListener {
    /**
	* Automatically generated unique class identifier.
	 */
	private static final long serialVersionUID = 5114598661300941143L;
	
	/**
	 * The root node of the list, the children of which are displayed as 
	 * list items.
	 */
	private Node mRoot = null;
	
	/**
	 * The document the list is contained in.
	 */
	private Document mDocument = null;
	
	/**
	 * The name of the elements which are considered list items.
	 */
	private String mElementName = null;
	
    /**
     * A string containing the XPath to the parent node.
     */
    private String mParentSelectedItemXPath = null;
    
	/**
	 * The name of the element containing the list.
     * TODO: Is this needed?
	 */
	private String mContainerElementName = null;
	
	/**
	 * Whether the children of this list are leaves of the DOM tree, if 
	 * not they have children with child nodes. This determines whether 
	 * the text content of nodes are set to the value of the list item.
	 */
	private boolean mChildrenAreLeaves = false;

	/**
     * A list which this list is the child of.
     * TODO: More docs here.
	 */
    private JList mParentList = null;
    
    /**
     * A list which this model is containing the data for.
     */
    private JList mContainerList = null;
    
	/**
	 * Constructor which sets the child element names.
	 * @param aElementName The name of the elements which are considered list items.
	 * @param aContainerElementName The name of the container element for the list.
	 * @param aChildrenAreLeaves Whether the elements of this list are leaves, if false they have
	 * node children. This determines whether the text content of the nodes should be set.
	 */
    public DOMListModel(JList aContainerList, String aElementName, String aContainerElementName, boolean aChildrenAreLeaves){
        // TODO: FIX and comment.
    	mContainerList = aContainerList;
        mContainerElementName = aContainerElementName;
        mParentSelectedItemXPath = mContainerElementName;
        mElementName = aElementName;
        mContainerElementName = aContainerElementName;
        mChildrenAreLeaves = aChildrenAreLeaves;
	}
    
    /**
     * Set the parent list of the list.
     * @param aParentList The parent list.
     */
    public void setParentList(JList aParentList) {
        mParentList = aParentList;
    }
    
    /**
     * Set a new document for the list.
     * @param aDocument The new document.
     */
	public void setDocument(Document aDocument){
        mDocument = aDocument;
        mRoot = null;
		
        // If the parent XPath starts with / this is a root level node
        // and can set its content now.
        if(mParentSelectedItemXPath.startsWith("/") && (mDocument != null)) { //$NON-NLS-1$
            // Set the value for the field. Query the DOM for a value
            // for this XPath. If there is not a value in the DOM the list
            // will be initialized to empty.
            Node result = Util.getResultNodeFromQuery(mDocument, mParentSelectedItemXPath);
            
            // Create the node for the list if there isn't one already.
            if( result == null) {
                result = Util.addNodesForXPath(mDocument, mParentSelectedItemXPath);
            }
            mRoot = result;
        }
        // Does this need to be on the Swing thread?
        fireContentsChanged(this, 0, getSize() - 1 );
    }
	
	/**
	 * Get the size of the list.
	 * @return The size of the list.
	 * @see javax.swing.ListModel#getSize()
	 */
       @Override public int getSize() {
		if(!canAccessDOM()){
			return 0;
		}
		
		// Count the child nodes which are elements.
		int size = 0;
		NodeList childNodes = mRoot.getChildNodes();
		for( int i = 0; i < childNodes.getLength(); ++i ){
			if(childNodes.item(i).getNodeType() == Node.ELEMENT_NODE){
				++size;
			}
		}
		return size;
	}

    /**
     * Determines if a given element exists in the list by searching the 
     * DOM.
     * @param aMatchObject An object for which to search the DOM.
     * @return Whether the item exists in the DOM.
     */
    @Override public boolean contains(Object aMatchObject) {
        // Check that the item we are searching for is null.
        if(aMatchObject == null){
            Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.1")); //$NON-NLS-1$
            return false;
        }
        
        // Check if the DOM can be accessed.
        if(!canAccessDOM()) {
            Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.2")); //$NON-NLS-1$
            return false;
        }
        

        // Create a new element and add a name attribute which is the
        // value of the object. This has to be done before the uniqueness
        // check so that searching for the item is easier.
        Element newElement = mRoot.getOwnerDocument().createElement(mElementName);
        newElement.setAttribute("name", aMatchObject.toString() ); //$NON-NLS-1$
        
        // If the children of the list are leaves set the string value
        // the text content.
        if(mChildrenAreLeaves){
            newElement.setTextContent(aMatchObject.toString());
        }
        
        // Search the DOM for the item.
        return (getDOMIndex(newElement) != -1);
    }
    
	/**
	 * Queries the list for an element at a given position. The element
	 * at this position may not reside at the same position in the DOM
	 * due to comment nodes.
	 * @see javax.swing.ListModel#getElementAt(int)
	 * @param aPosition in the list to return the element at.
	 */
    @Override public Object getElementAt(int aPosition) {
		if(!canAccessDOM() || aPosition < 0){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.4")); //$NON-NLS-1$
			return null;
		}
		
		// Find the actual position ignoring non element nodes.
		NodeList children = mRoot.getChildNodes();
		if(children != null){
			int trueNumber = -1;
			for(int i = 0; i < children.getLength(); ++i){
				// Increment the actual child count if this location
				// contains an element node.
				if(children.item(i).getNodeType() == Node.ELEMENT_NODE){
					++trueNumber;
				}
				
				// Check if the actual position is the desired position. This
				// will only be true for element nodes given that trueNumber is
				// initialized to negative 1.
				if(trueNumber == aPosition){
					// Return a wrapped DOM node which overrides the toString method
					// so it reports the correct value to the list.
					return NodeWrapper.createProxy(children.item(i));
				}
			}
		}
		Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.5")); //$NON-NLS-1$
		return null;
	}
	
	/**
	 * Add a single list item to the list. This operation adds the objects string
	 * value to the list as the name attribute of a new node in the DOM. If this list
	 * has children which are leaves this will also set the text content of the new
	 * node to the same value. This will only succeed if the new item does not have the
	 * same value as an existing item in the list.
     * TODO: Figure out how to signal an error.
	 * @param aObject New list item to add to the list.
	 */
    @Override public void addElement(Object aObject){
		// Check that the item we are attemption to add is not null.
		if(aObject == null){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.6")); //$NON-NLS-1$
			return;
		}
		
        // Check if the DOM can be accessed.
        if(!canAccessDOM()) {
            Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.7")); //$NON-NLS-1$
            return;
        }
        
        // Create a new element and add a name attribute which is the
        // value of the object. This has to be done before the uniqueness
        // check so that searching for the item is easier.
        Element newElement = mRoot.getOwnerDocument().createElement(mElementName);
        newElement.setAttribute("name", aObject.toString() ); //$NON-NLS-1$
        
        // If the children of the list are leaves set the string value
        // the text content.
        if(mChildrenAreLeaves){
            newElement.setTextContent(aObject.toString());
        }
		// Check if the name is unique.
		if(getDOMIndex(newElement) != -1){
            // This interface limits our ability to return errors. The caller
            // should have checked uniqueness before requesting the add operation.
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.9")); //$NON-NLS-1$
			return;
		}
		
		// Add the new child to the list.
		mRoot.appendChild(newElement);
		
		// Notify that the list changed, which is required for GUI updates.
		int addedItemListIndex = getListIndex(newElement);
		fireIntervalAdded(this, addedItemListIndex, addedItemListIndex);
	}
	
	/**
	 * Return whether this list is ready to have elements added to it.
	 * @return Whether the list can have items added onto it.
	 */
    public boolean canAddElements(){
		return canAccessDOM();
	}
	
    /**
     * Remove an element from the list.
     * @param aObject Object to remove from the list.
     */
    @Override public boolean removeElement(Object aObject){
        // Check if a null element was passed in as the item to remove.
        if(aObject == null) {
            Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.10")); //$NON-NLS-1$
            return false;
        }
		// Find the index within the DOM of the item.
		int domIndex = getDOMIndex(aObject);
		
        // TODO: Evaluate if better return values could be found for errors.
		// Check if the item could not be found.
		if(domIndex == -1){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.11") + aObject.toString() + Messages.getString("DOMListModel.12")); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		}
		
		// Remove the object.
		int removedListIndex = getListIndex(aObject);
		mRoot.removeChild(mRoot.getChildNodes().item(domIndex));
		
		// Alert the list that part of it changed. Uses list indexes,
		// not DOM indexes.
		fireIntervalRemoved(this, removedListIndex, removedListIndex);
		return true;
	}
	
    /**
     * Move an element back a single position in the list.
     * @param aObject Element to move back in the list.
     * @return The new index of the element.
     */
	public int moveElementBack(Object aObject){
        // Check for a null element, this should not happen.
        if(aObject == null) {
            Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.13")); //$NON-NLS-1$
            return 0;
        }
		// Find the index within the DOM of the item.
		int domIndex = getDOMIndex(aObject);
        
		// Check if the item could not be found.
        // TODO: Evaluate if better return values could be found for errors.
		if(domIndex == -1){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.14") + aObject.toString() + Messages.getString("DOMListModel.15")); //$NON-NLS-1$ //$NON-NLS-2$
			return 0;
		}
		
		// Check if its the first item in the list, which cannot be moved back.
		if(domIndex == 0){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.16")); //$NON-NLS-1$
			return 0;
		}
		
		// Move the item back.
		NodeList children = mRoot.getChildNodes();
		Node curr = children.item(domIndex);
		Node prev = getItemBefore(curr);
		curr = mRoot.removeChild(curr);
		mRoot.insertBefore(curr, prev);
		
		// Fire an event off that the range between the old position of the item
		// and the new position of the item changed.
		fireContentsChanged(this, getListIndex(curr), getListIndex(prev));
		
		// Return the new location of the item so it is still highlighted.
		// Determine the new list index of the moved item. This is
		// not always the DOM index.
		return getListIndex(curr);
	}
	
    /**
     * Move an element up a single position in the list.
     * @param aObject Element to move up in the list.
     * @return The new index of the element.
     */
	public int moveElementForward(Object aObject){
        // Check for a null element, this should not happen.
        if(aObject == null) {
            Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.17")); //$NON-NLS-1$
            return 0;
        }
        
		// Find the index within the DOM of the item.
		int domIndex = getDOMIndex(aObject);
        
		// Check if the item could not be found.
		if(domIndex == -1){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.18") + aObject.toString() + Messages.getString("DOMListModel.19")); //$NON-NLS-1$ //$NON-NLS-2$
			return 0;
		}
		
		// Check if its the last item in the list, which cannot be moved up.
		if(domIndex == getSize() - 1 ){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.0")); //$NON-NLS-1$
			return 0;
		}
		NodeList children = mRoot.getChildNodes();
		Node curr = children.item(domIndex);
		// Find a node to insert this item before. If its the second to
		// last item, the item to insert before is null because there
		// won't be an item after it.
		Node prev = getItemAfter(getItemAfter(curr));
		curr = mRoot.removeChild(curr);
		mRoot.insertBefore(curr, prev);
		fireContentsChanged(this, getListIndex(getItemBefore(curr)), getListIndex(prev));
		
		// Return the new location of the item so it is still highlighted.
		// Determine the new list index of the moved item. This is
		// not related to the DOM index.
		return getListIndex(curr);
	}
    
	/**
	 * Get the previous list item starting at an item.
	 * @param aItem The list item to find the item before.
	 * @return The previous list item, null if there is not one.
	 */
	private Node getItemBefore(Node aItem){
		// Get the index of the node.
		int domIndex = getDOMIndex(aItem);
		
		// Iterate backwards and search for a list item.
		NodeList children = mRoot.getChildNodes();
		for(int i = domIndex - 1; i >= 0; --i){
			// Check if the item is a element node, which represent a list item.
			if(children.item(i).getNodeType() == Node.ELEMENT_NODE){
				return children.item(i);
			}
		}
		// If the search failed return null, calling function will handle this.
		return null;
	}
	
	/**
	 * Get the next list item starting at an item.
	 * @param aItem The list item to find the item after.
	 * @return The next list item, null if there is not one.
	 */
	private Node getItemAfter(Node aItem){
		// Get the index of the node.
		int domIndex = getDOMIndex(aItem);
		
		// Iterate forwards and search for a list item.
		NodeList children = mRoot.getChildNodes();
		for(int i = domIndex + 1; i < children.getLength(); ++i){
			// Check if the item is a element node, which represent a list item.
			if(children.item(i).getNodeType() == Node.ELEMENT_NODE){
				return children.item(i);
			}
		}
		// If the search failed return null, calling function will handle this.
		return null;
	}
	
	/**
	 * Helper function to find a single item in the list.
	 * @param aListItem A list item to find.
	 * @return The index into the DOM of the item, -1 if an error occurs
	 * or the item cannot be found.
	 */
	private int getDOMIndex(Object aListItem){
		// Check if this operation can be performed.
		if(!canAccessDOM() ){
			Logger.global.log(Level.WARNING,
				Messages.getString("DOMListModel.28")); //$NON-NLS-1$
			return 0;
		}

		if(aListItem == null || !(aListItem instanceof Node)){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.22")); //$NON-NLS-1$
		}
	
		// Get the name of the new object.
		String newNodeName = Util.getNameAttrValue((Node)aListItem);
		NodeList children = mRoot.getChildNodes();

		for(int i = 0; i < children.getLength(); ++i){
			String name = Util.getNameAttrValue(children.item(i));
			// Check if the child matches the requested element.
			if((name != null) && name.equals(newNodeName)){
				return i;
			}
		}
		// The item was not found. Let the caller handle printing an error message.
		return -1;
	}
	
	/**
	 * Helper function to find the list index of an item.
	 * @param aListItem A item to find the list index of.
	 * @return The index into the list of the item, -1 if an error occurs
	 * or the item cannot be found.
	 */
	private int getListIndex(Object aListItem){
		// Check if this operation can be performed.
		if(!canAccessDOM() ){
			Logger.global.log(Level.WARNING,
				Messages.getString("DOMListModel.23")); //$NON-NLS-1$
			return 0;
		}

		if(aListItem == null || !(aListItem instanceof Node)){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListModel.24")); //$NON-NLS-1$
		}
		
		// Get the name of the new object.
		String newNodeName = Util.getNameAttrValue((Node)aListItem);
		NodeList children = mRoot.getChildNodes();
		
		// Keep track of the number of elements, or list items found.
		int listIndex = -1;
		for(int i = 0; i < children.getLength(); ++i){
			// Check if the item is a list item, or element.
			if(children.item(i).getNodeType() == Node.ELEMENT_NODE){
				++listIndex;
			}
			else {
				// Don't check the names of non-element nodes.
				continue;
			}
			String name = Util.getNameAttrValue(children.item(i));
			// Check if the child matches the requested element.
			if((name != null) && name.equals(newNodeName)){
				return listIndex;
			}
		}
		// The item was not found. Let the caller handle printing an error message.
		return -1;
	}
	
	/**
	 * An internal method which determines whether the current state of
	 * the object allows methods which access the internal DOM tree to be
	 * called
	 * 
	 * @return Whether access to internal data is currently allowed.
	 */
	private boolean canAccessDOM() {
		return (mRoot != null);
	}
    
    /**
     * Method to return the XPath of the currently selected item.
     */
    public String getSelectedXPath(int aElementIndex) {
        return mParentSelectedItemXPath + "/" + mElementName + "[@name='"  //$NON-NLS-1$ //$NON-NLS-2$
        + Util.getNameAttrValue((Node)getElementAt(aElementIndex)) + "']"; //$NON-NLS-1$
    }
    
    /**
     * Method called when the selection in the parent list is changed.
     * @param aEvent The even received.
     */
    public void valueChanged(ListSelectionEvent aEvent) {
        mContainerList.setSelectedIndex(-1);
        update(((JList)aEvent.getSource()).getSelectedIndex());
    }
    
    /**
     * Method called when an interval in the parent list is added.
     * @param aEvent The event received.
     */
    public void intervalAdded(ListDataEvent aEvent) {
        // update(-1);
    }

    /**
     * Method called when an interval is removed from the parent list.
     * @param aEvent aEvent The event received.
     */
    public void intervalRemoved(ListDataEvent aEvent) {
        // update(-1);
    }
    
    /**
     * Method called when the contents of the parent list is changed.
     * @param aEvent The event received.
     */
    public void contentsChanged(ListDataEvent aEvent) {
        mContainerList.setSelectedIndex(-1);
        update(((DOMListModel)aEvent.getSource()).mContainerList.getSelectedIndex());
    }
    
    /**
     * Internal method which updates the list based on an event
     * received from a parent list.
     * @param aNewIndex The index now selected.
     */
    private void update(int aNewIndex){
       mParentSelectedItemXPath = ((DOMListModel)mParentList.getModel()).getSelectedXPath(aNewIndex);

       // Set the value for the field. Query the DOM for a value
       // for this XPath. If there is not a value in the DOM the list
       // will be initialized to empty.
       Node result = Util.getResultNodeFromQuery(mDocument, mParentSelectedItemXPath);
       
       // Create the node for the list if there isn't one already.
       if( result == null) {
           result = Util.addNodesForXPath(mDocument, mParentSelectedItemXPath);
       }
       mRoot = result;
       fireContentsChanged(this, 0, getSize() - 1 );
    }
}
