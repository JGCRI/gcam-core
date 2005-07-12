/**
 * 
 */
package guicomponents;

import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import utils.DOMUtils;
import utils.NodeWrapper;

/**
 * @author Josh Lurz
 *
 */
public class DOMTreeModel implements TreeModel {
    /**
     * The Document containing the tree. This may be changed
     * by calls to set document.
     */
    Document mDocument = null;
    
    /**
     * A list of tree event listeners.
     */
    Vector<TreeModelListener> mListeners = null;
    
    /**
     * Map of cached node wrappers.
     */
    private Map<Node,Object> mCachedNodeWrappers = null;
    
    /**
     * The name of leaf elements in the DOM.
     */
    private String mLeafName = null;
    
    /**
     * Constructor which initializes the root node in the tree.
     * @param aDocument The document containing the tree.
     * @param aLeafName The name of leaves in the tree.
     */
    public DOMTreeModel(Document aDocument, String aLeafName) {
        super();
        mDocument = aDocument;
        mLeafName = aLeafName;
        mCachedNodeWrappers = new HashMap<Node, Object>();
        mListeners = new Vector<TreeModelListener>();
    }

    /**
     * Set the document containing the tree.
     * @param aDocument The new document.
     */
    public void setDocument(Document aDocument) {
        // Store the root so we can use it to dispatch a tree node
        // removed event.
        Object oldRoot = getRoot();
        
        // Set the document.
        mDocument = aDocument;
        
        // Clear the cached list of nodes.
        mCachedNodeWrappers.clear();
        
        // Fire event that the root is removed if there was an existing root.
        // This may not be enough.
        if(oldRoot != null) {
            fireTreeNodesRemoved(new TreeModelEvent(this, new TreePath(oldRoot), new int[] {0}, new Object[]{oldRoot} ));
        }
        // Fire event that a new root was added if there is a new one.
        // This may not be enough.
        Object newRoot = getRoot();
        if(newRoot != null) {
            fireTreeNodesInserted(new TreeModelEvent(this, new TreePath(newRoot), new int[] {0}, new Object[]{newRoot} ));
        }
    }
    
    /**
     * The root node in the tree.
     * @see javax.swing.tree.TreeModel#getRoot()
     */
    public Object getRoot() {
        if(mDocument == null) {
            return null;
        }
        return getOrCreateWrapper(mDocument.getDocumentElement());
    }

    /**
     * Get the child of a given node at a specific index.
     * @param aParent Parent of which to find the child.
     * @param aChildIndex Index of the child to return.
     * @return The node at the requested index, null if the index
     * is invalid.
     * @see javax.swing.tree.TreeModel#getChild(java.lang.Object, int)
     */
    public Object getChild(Object aParent, int aChildIndex) {
        // Get the DOM index of the list index.
        int DOMIndex = DOMUtils.getDOMIndexForListIndex((Node)aParent, aChildIndex);
        
        // If the DOM index is -1 than the parent or child is invalid.
        if(DOMIndex == -1) {
            return null;
        }
        // Return the wrapped child node.
        return getOrCreateWrapper(((Node)aParent).getChildNodes().item(DOMIndex));
    }

    /**
     * Returns the number of children a node has.
     * @param aNode Node for which to return the child count.
     * @return The number of children of the node.
     * @see javax.swing.tree.TreeModel#getChildCount(java.lang.Object)
     */
    public int getChildCount(Object aNode) {
        return DOMUtils.getNumberOfElementChildren((Node)aNode);
    }

    /**
     * Returns whether the node is a leaf.
     * @return Whether the node is a leaf.
     */
    public boolean isLeaf(Object aNode) {
        return ((Node)aNode).getNodeName().equals(mLeafName);
    }

    /**
     * The method called when the user has modified a value at a specific
     * node. Alerts listeners if the value is actually changed.
     * @param aPath Object in the tree who's value has been modified.
     * @param aNewValue The new value of the specified node.
     * @see javax.swing.tree.TreeModel#valueForPathChanged(javax.swing.tree.TreePath, java.lang.Object)
     */
    public void valueForPathChanged(TreePath aPath, Object aNewValue) {
        // Path items are nodes, so just find the last item and check its 
        // value.
        Node lastNode = (Node)aPath.getLastPathComponent();
        
        // Check if the current content is equal to the new content.
        if(lastNode.getTextContent().equals(aNewValue)) {
            // Don't notify the listeners.
            return;
        }
        // Set the new content and notify the listeners.
        String newValue = aNewValue != null ? aNewValue.toString():"";
        lastNode.setTextContent(newValue);
        // Notify listeners.
        int DOMIndex = DOMUtils.getDOMIndexOfObject(lastNode.getParentNode(), lastNode);
        fireTreeNodesChanged(new TreeModelEvent(this, aPath, new int[] {DOMIndex}, new Object[]{lastNode} ));
    }

    /**
     * Returns the index of a child within a parent.
     * @return The index of the child within the parent.
     * @see javax.swing.tree.TreeModel#getIndexOfChild(java.lang.Object, java.lang.Object)
     */
    public int getIndexOfChild(Object aParent, Object aChild) {
        // The caller expects to receive the -1 return value
        // for invalid parents and children.
        return DOMUtils.getDOMIndexOfObject((Node)aParent, aChild);
        
    }

    /**
     * Add a tree model listener to the tree.
     * @param aListener The tree model listener to add.
     * @see javax.swing.tree.TreeModel#addTreeModelListener(javax.swing.event.TreeModelListener)
     */
    public void addTreeModelListener(TreeModelListener aListener) {
        // Add the listener to the end of the list.
        mListeners.add(aListener);
    }

    /**
     * Removes a tree model listener from the tree.
     * @param aListener The listener to remove.
     * @see javax.swing.tree.TreeModel#removeTreeModelListener(javax.swing.event.TreeModelListener)
     */
    public void removeTreeModelListener(TreeModelListener aListener) {
        // Remove the listener.
        mListeners.remove(aListener);
    }
    
    /**
     * Returns a DOM node wrapped in a node wrapper. The node wrapper
     * may be from the cached node set or it may be newly created.
     * @param aNode A DOM node to wrap.
     * @return A wrapper around the DOM node which overrides the toString
     * method to return the name attribute of the node.
     */
    private Object getOrCreateWrapper(Node aNode) {
        // Check if the node has already had a wrapper created for it.
        Object wrapper = mCachedNodeWrappers.get(aNode);
        
        // If the wrapper is null than one must be created for the
        // element node and cached. This is so a wrappers of the same
        // element node will always be equal.
        if(wrapper == null ){
            wrapper = NodeWrapper.createProxy(aNode);
            mCachedNodeWrappers.put(aNode, wrapper);
        }
        
        // Return the wrapper.
        return wrapper;
    }
    
    /**
     * Fire a tree nodes changed event.
     * @param aEvent The tree model event.
     */
    private void fireTreeNodesChanged(TreeModelEvent aEvent) {
        // Iterate over the listeners and notify each.
        for(int i = 0; i < mListeners.size(); ++i) {
            mListeners.get(i).treeNodesChanged(aEvent);
        }
    }
    /**
     * Fire a tree nodes inserted event.
     * @param aEvent The tree model event.
     */
    private void fireTreeNodesInserted(TreeModelEvent aEvent) {
        // Iterate over the listeners and notify each.
        for(int i = 0; i < mListeners.size(); ++i) {
            mListeners.get(i).treeNodesInserted(aEvent);
        }
    }
    
    /**
     * Fire a tree nodes changed event.
     * @param aEvent The tree model event.
     */
    private void fireTreeNodesRemoved(TreeModelEvent aEvent) {
        // Iterate over the listeners and notify each.
        for(int i = 0; i < mListeners.size(); ++i) {
            mListeners.get(i).treeNodesRemoved(aEvent);
        }
    }
}
