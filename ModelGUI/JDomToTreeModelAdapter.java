/*
 * JDomToTreeModelAdapter.java - this file is incomplete!!!!!
 *
 * Created on May 20, 2003, 3:40 PM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
//import java.io.*;
import java.util.*;
import java.awt.Dimension;
import org.jdom.*;

// This adapter converts a JDOM into a JTree model.
public class JDomToTreeModelAdapter implements javax.swing.tree.TreeModel { //extends DefaultTreeModel { //
    private Document document; 
    private List listenerList;
    
    /*public JDomToTreeModelAdapter(Document doc) {
        super((TreeNode)new AdapterNode(doc.getRootElement()));
        document = doc;
        listenerList = new LinkedList();
    }*/
    
    public JDomToTreeModelAdapter(Document doc) {
        document = doc;
        listenerList = new LinkedList();
    }
    
    //Returns the root of the tree
    public Object getRoot() {
        Element root = document.getRootElement();
        return new AdapterNode(root);
    }
    
    //Returns true if node is a leaf
    public boolean isLeaf(Object aNode) {
        // Determines whether the icon shows up to the left.
        // Return true for any node with no children
        // by getting a list of all chidren and checking if it's empty
        AdapterNode node = (AdapterNode)aNode;
        if (node.childCount() == 0)
            return true;
        else
            return false;
    }
    
    //Returns the number of children of parent
    public int getChildCount(Object parent) {
        AdapterNode node = (AdapterNode)parent;
        return node.childCount();
    }
    
    //Returns the child of parent at index index in the parent's child array
    public Object getChild(Object parent, int index) {        
        AdapterNode node = (AdapterNode)parent;
        return node.child(index);
    }
    
    // Returns the index of child in parent
    public int getIndexOfChild(Object parent, Object child) {
        AdapterNode node = (AdapterNode)parent;
        return node.index(child);
    }
    
    public void valueForPathChanged(TreePath path, Object newValue) {
        Object oldValue = path.getLastPathComponent();
        //if the value of the node has not changed, do nothing
        if(oldValue.equals(newValue)) return;
        
        TreeModelListener listener;
        Iterator it = listenerList.iterator();
        while (it.hasNext()) {
            listener = (TreeModelListener)it.next();
            listener.treeNodesChanged(new TreeModelEvent(this, path.getParentPath()));
        }
        return;        
    }
    
    public void addTreeModelListener(TreeModelListener listener) {
        if ( listener != null ) {
            listenerList.add(listener);
        }
    }
    public void removeTreeModelListener(TreeModelListener listener) {
        if ( listener != null ) {
            listenerList.remove(listener);
        }
    }
    
    public void insertNodeInto(AdapterNode newChild, TreePath parentPath, int index) {
        AdapterNode parent = (AdapterNode)parentPath.getLastPathComponent();

        int[] arr1 = {parent.addChild(newChild)};
        AdapterNode[] arr2 = {newChild};
        fireTreeNodesInserted(new TreeModelEvent(this, parentPath, arr1, arr2));
    }
    
    public void removeNodeFrom(TreePath nodePath) {
        AdapterNode node = (AdapterNode)nodePath.getLastPathComponent();
        AdapterNode parent = node.getParent();
        
        int[] arr1 = {parent.removeChild(node.getName())};
        AdapterNode[] arr2 = {node};
        fireTreeNodesRemoved(new TreeModelEvent(this, nodePath.getParentPath(), arr1, arr2));
    }
    
    public void fireTreeNodesChanged( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while ( listeners.hasNext() ) {
            TreeModelListener listener = (TreeModelListener) listeners.next();
            listener.treeNodesChanged( e );
        }
    }
    public void fireTreeNodesInserted( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while ( listeners.hasNext() ) {
            TreeModelListener listener = (TreeModelListener) listeners.next();
            listener.treeNodesInserted( e );
        }
    }
    public void fireTreeNodesRemoved( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while ( listeners.hasNext() ) {
            TreeModelListener listener = (TreeModelListener) listeners.next();
            listener.treeNodesRemoved( e );
        }
    }
    public void fireTreeStructureChanged( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while (listeners.hasNext()) {
            TreeModelListener listener = (TreeModelListener)listeners.next();
            listener.treeStructureChanged(e);
        }
    } 
}
