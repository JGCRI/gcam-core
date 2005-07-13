/**
 * 
 */
package guicomponents;

import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.w3c.dom.Node;

import utils.DOMUtils;
import utils.TreeNodeWrapper;

/**
 * @author Josh Lurz
 *
 */
public class DOMTreeModel implements TreeModel {
    /**
     * The immutable root node of the tree.
     */
    private final transient Node mRoot;
    
    /**
     * A list of tree event listeners.
     */
    final transient ArrayList<TreeModelListener> mListeners;
    
    /**
     * Map of cached node wrappers.
     */
    private transient final Map<Node,Object> mWrappers;
    
    /**
     * The name of leaf elements in the DOM.
     */
    private transient final String mLeafName;
    
    /**
     * Constructor which initializes the root node in the tree.
     * @param aRootNode The root node of the tree.
     * @param aLeafName The name of leaves in the tree.
     */
    public DOMTreeModel(Node aRootNode, String aLeafName) {
        super();
        mRoot = aRootNode;
        mLeafName = aLeafName;
        mWrappers = new HashMap<Node, Object>();
        mListeners = new ArrayList<TreeModelListener>();
    }
    
    /**
     * The root node in the tree.
     * @see javax.swing.tree.TreeModel#getRoot()
     */
    public Object getRoot() {
        return getOrCreateWrapper(mRoot);
    }

    /**
     * Get the child of a given node at a specific index.
     * @param aParent Parent of which to find the child.
     * @param aChildIndex Index of the child to return.
     * @return The node at the requested index, null if the index
     * is invalid.
     * @see javax.swing.tree.TreeModel#getChild(java.lang.Object, int)
     */
    public Object getChild(final Object aParent, final int aChildIndex) {
        // Get the DOM index of the list index.
        final int DOMIndex = DOMUtils.getDOMIndexForListIndex((Node)aParent, aChildIndex);
        
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
    public int getChildCount(final Object aNode) {
        return DOMUtils.getNumberOfElementChildren((Node)aNode);
    }

    /**
     * Returns whether the node is a leaf.
     * @param aNode Node to determine if it is a leaf.
     * @return Whether the node is a leaf.
     */
    public boolean isLeaf(final Object aNode) {
        return ((Node)aNode).getNodeName().equals(mLeafName);
    }

    /**
     * The method called when the user has modified a value at a specific
     * node. Alerts listeners if the value is actually changed.
     * @param aPath Object in the tree who's value has been modified.
     * @param aNewValue The new value of the specified node.
     * @see javax.swing.tree.TreeModel#valueForPathChanged(javax.swing.tree.TreePath, java.lang.Object)
     */
    public void valueForPathChanged(final TreePath aPath, final Object aNewValue) {
        // Path items are nodes, so just find the last item and check its 
        // value.
        final Node oldNode = (Node)aPath.getLastPathComponent();
        final Node newNode = (Node)aNewValue;
        if(oldNode.equals(newNode)){
        	return;
        }
        
        // Check if the old and new nodes are the same node but modified.
        if(oldNode.isSameNode(newNode)){
        	// Notify listeners that the node changed.
            final int listIndex = DOMUtils.getListIndexOfObject(newNode.getParentNode(), newNode);
        	fireTreeNodesChanged(new TreeModelEvent(this, aPath, new int[] {listIndex}, new Object[]{getOrCreateWrapper(newNode)} ));
        }
        // Otherwise a node was added.
        else {
        	fireTreeStructureChanged(new TreeModelEvent(getOrCreateWrapper(newNode.getParentNode()), aPath));
        }
    }

    /**
     * Returns the index of a child within a parent.
     * @return The index of the child within the parent.
     * @see javax.swing.tree.TreeModel#getIndexOfChild(java.lang.Object, java.lang.Object)
     */
    public int getIndexOfChild(final Object aParent, final Object aChild) {
        // The caller expects to receive the -1 return value
        // for invalid parents and children.
        return DOMUtils.getDOMIndexOfObject((Node)aParent, aChild);
        
    }

    /**
     * Add a tree model listener to the tree.
     * @param aListener The tree model listener to add.
     * @see javax.swing.tree.TreeModel#addTreeModelListener(javax.swing.event.TreeModelListener)
     */
    public void addTreeModelListener(final TreeModelListener aListener) {
        // Add the listener to the end of the list.
        mListeners.add(aListener);
    }

    /**
     * Removes a tree model listener from the tree.
     * @param aListener The listener to remove.
     * @see javax.swing.tree.TreeModel#removeTreeModelListener(javax.swing.event.TreeModelListener)
     */
    public void removeTreeModelListener(final TreeModelListener aListener) {
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
    private Object getOrCreateWrapper(final Node aNode) {
        // Check if the node has already had a wrapper created for it.
        Object wrapper = mWrappers.get(aNode);
        
        // If the wrapper is null than one must be created for the
        // element node and cached. This is so a wrappers of the same
        // element node will always be equal.
        if(wrapper == null ){
            wrapper = TreeNodeWrapper.createProxy(aNode);
            mWrappers.put(aNode, wrapper);
        }
        
        // Return the wrapper.
        return wrapper;
    }
    
    /**
     * Fire a tree nodes changed event.
     * @param aEvent The tree model event.
     */
    private void fireTreeNodesChanged(final TreeModelEvent aEvent) {
        // Iterate over the listeners and notify each.
        for(int i = 0; i < mListeners.size(); ++i) {
            mListeners.get(i).treeNodesChanged(aEvent);
        }
    }
    
    /**
     * Fire a tree structure changed event.
     * @param aEvent The tree model event.
     */
    private void fireTreeStructureChanged(final TreeModelEvent aEvent) {
        // Iterate over the listeners and notify each.
        for(int i = 0; i < mListeners.size(); ++i) {
            mListeners.get(i).treeStructureChanged(aEvent);
        }
    }
}
