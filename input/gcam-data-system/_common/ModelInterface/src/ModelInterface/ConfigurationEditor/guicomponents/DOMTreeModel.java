/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
package ModelInterface.ConfigurationEditor.guicomponents;

import ModelInterface.ConfigurationEditor.guihelpers.TreeNodeWrapper;

import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.w3c.dom.Node;

import ModelInterface.ConfigurationEditor.utils.DOMUtils;

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
    private final transient ArrayList<TreeModelListener> mListeners;
    
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
        final Node newNode = (Node)getOrCreateWrapper((Node)aNewValue);
        
        // Check if the old and new nodes are the same node but modified.
        if(oldNode.isSameNode(newNode)){
        	// Notify listeners that the node changed.
            final int listIndex = DOMUtils.getListIndexOfObject(newNode.getParentNode(), newNode);
        	fireTreeNodesChanged(new TreeModelEvent(this, aPath, new int[] {listIndex}, new Object[]{getOrCreateWrapper(newNode)} ));
        }
        // Otherwise a node was added or deleted.
        else {
        	fireTreeStructureChanged(new TreeModelEvent(oldNode, aPath));
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
