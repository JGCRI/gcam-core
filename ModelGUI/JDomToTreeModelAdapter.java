/*
 * JDomToTreeModelAdapter.java - this file is incomplete!!!!!
 *
 * Created on May 20, 2003, 3:40 PM
 */

package ModelGUI;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.util.*;
import java.awt.Dimension;
import org.jdom.*;

/** This class allows a JDOM tree to be displayed and manipulated using 
 * a JTree widget. The JDOM tree, in tern, is read in from an XML file.
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */
public class JDomToTreeModelAdapter implements javax.swing.tree.TreeModel { //extends DefaultTreeModel { //
    private Document document; 
    private List listenerList;
        
    /** Creates a new instance of the class.
     *
     * @param doc the Document that specifies the XML file to be read */    
    public JDomToTreeModelAdapter(Document doc) {
        document = doc;
        listenerList = new LinkedList();
    }
    
    /** Retrieves the root of the tree.
     *
     * @return AdapterNode representation of the root */    
    public Object getRoot() {
        Element root = document.getRootElement();
        return new AdapterNode(root);
    }
    
    /** Retrieves whether current node has any children.
     *
     * @param aNode AdapterNode representation of target node
     * @return true if node has no children */    
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
    
    /** Retrrieves the number of children of current node.
     *
     * @param parent AdapterNode representation of desired node
     * @return the child count */    
    public int getChildCount(Object parent) {
        AdapterNode node = (AdapterNode)parent;
        return node.childCount();
    }
    
    /** Retrieves the specified child of <code> parent </code>.
     *
     * @param parent AdapterNode representation of target parent
     * @param index index into parent's child array
     * @return AdapterNode representation of the child at <code> index </code> */    
    public Object getChild(Object parent, int index) {        
        AdapterNode node = (AdapterNode)parent;
        return node.child(index);
    }
    
    // Returns the index of child in parent
    /**
     * @param parent
     * @param child
     * @return  */    
    public int getIndexOfChild(Object parent, Object child) {
        AdapterNode node = (AdapterNode)parent;
        return node.index(child);
    }
    
    /** Gets called when the value of a node at the end of <code> path </code>
     * changes.
     *
     * @param path the TreePath of the selected node
     * @param newValue the value that the node at <code> path </code> must be 
     *      changed to */    
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
    
    /** Adds a TreeModelListener that will respond to user input and 
     * manipulation of the tree.
     *
     * @param listener specific TreeModelListener */    
    public void addTreeModelListener(TreeModelListener listener) {
        if ( listener != null ) {
            listenerList.add(listener);
        }
    }
    
    /** Removes the specified TreeModelListener. This component no longer 
     * sends out notifications of changes in the tree. \
     *
     * @param listener specific TreeModelListener to remove*/    
    public void removeTreeModelListener(TreeModelListener listener) {
        if ( listener != null ) {
            listenerList.remove(listener);
        }
    }
    
    /** Adds a new node to the tree in a specified location.
     *
     * @param newChild the object that will be added
     * @param parentPath path to the parent node of <code> newChild </code>
     * @param index index into the child array of the parent that specifies the
     *      location where <code> newChild </code> is to be inserted */    
    public void insertNodeInto(AdapterNode newChild, TreePath parentPath, int index) {
        AdapterNode parent = (AdapterNode)parentPath.getLastPathComponent();

        int[] arr1 = {parent.addChild(newChild)};
        AdapterNode[] arr2 = {newChild};
        fireTreeNodesInserted(new TreeModelEvent(this, parentPath, arr1, arr2));
    }
    
    /** Deletes a node from the tree.
     *
     * @param nodePath the path of the node that is to be deleted */    
    public void removeNodeFrom(TreePath nodePath) {
        AdapterNode node = (AdapterNode)nodePath.getLastPathComponent();
        AdapterNode parent = node.getParent();
        
        int[] arr1 = {parent.removeChild(node.getName())};
        AdapterNode[] arr2 = {node};
        fireTreeNodesRemoved(new TreeModelEvent(this, nodePath.getParentPath(), arr1, arr2));
    }
    
    /** Sends a notification that value(s) of node(s) of the tree have been modified. 
     * These notifications are picked up by registered TreeModelListener objects.
     * @see fireTreeNodesInserted(TreeModelEvent e)
     * @see fireTreeNodesRemoved(TreeModelEvent e)
     * @see fireTreeStructureChanged(TreeModelEvent e)
     *
     * @param e user generated or automatic event */    
    public void fireTreeNodesChanged( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while ( listeners.hasNext() ) {
            TreeModelListener listener = (TreeModelListener) listeners.next();
            listener.treeNodesChanged( e );
        }
    }
    
    /** Sends a notification that a node has been inserted into the tree.
     * @see fireTreeNodesRemoved(TreeModelEvent e)
     * @see fireTreeNodesChanged(TreeModelEvent e)
     * @see fireTreeStructureChanged(TreeModelEvent e)
     *
     * @param e user generated or automatic event that cause the insertion */    
    public void fireTreeNodesInserted( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while ( listeners.hasNext() ) {
            TreeModelListener listener = (TreeModelListener) listeners.next();
            listener.treeNodesInserted( e );
        }
    }
    
    /** Sends a notification that a node has been deleted from the tree.
     * @see fireTreeNodesInserted(TreeModelEvent e)
     * @see fireTreeNodesChanged(TreeModelEvent e)
     * @see fireTreeStructureChanged(TreeModelEvent e)
     *
     * @param e user generated or automatic event that cause the deletion */    
    public void fireTreeNodesRemoved( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while ( listeners.hasNext() ) {
            TreeModelListener listener = (TreeModelListener) listeners.next();
            listener.treeNodesRemoved( e );
        }
    }
    
    /** Sends a notification that the tree's structure has been altered.
     *@ see fireTreeNodesInserted(TreeModelEvent e)
     * @see fireTreeNodesChanged(TreeModelEvent e)
     * @see fireTreeNodesChanged(TreeModelEvent e)
     *
     * @param e user generated or automatic event */    
    public void fireTreeStructureChanged( TreeModelEvent e ) {
        Iterator listeners = listenerList.iterator();
        while (listeners.hasNext()) {
            TreeModelListener listener = (TreeModelListener)listeners.next();
            listener.treeStructureChanged(e);
        }
    } 
}
