/*
 * DomToTreeModelAdapter.java
 *
 * Created on June 2, 2003, 4:01 PM
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
import org.w3c.dom.*;

// This adapter converts a JDOM into a JTree model.
public class DomToTreeModelAdapter implements javax.swing.tree.TreeModel {
    private Document document; 
    private List listenerList;
    
    public DomToTreeModelAdapter(Document doc) {
        document = doc;
        listenerList = new LinkedList();
    }
    
    //Returns the root of the tree
    public Object getRoot() {
        return new WrappedNode(document);
    }
    
    //Returns true if node is a leaf
    public boolean isLeaf(Object aNode) {
        // Determines whether the icon shows up to the left.
        // Return true for any node with no children
        // by getting a list of all chidren and checking if it's empty
        WrappedNode node = (WrappedNode)aNode;
        if (node.childCount() == 0)
            return true;
        else
            return false;
    }
    
    //Returns the number of children of parent
    public int getChildCount(Object parent) {
        WrappedNode node = (WrappedNode)parent;
        return node.childCount();
    }
    
    //Returns the child of parent at index index in the parent's child array
    public Object getChild(Object parent, int index) {        
        WrappedNode node = (WrappedNode)parent;
        return node.child(index);
    }
    
    // Returns the index of child in parent
    public int getIndexOfChild(Object parent, Object child) {
        WrappedNode node = (WrappedNode)parent;
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
    
      /*
       * Invoke these methods to inform listeners of changes.
       * (Not needed for this example.)
       * Methods taken from TreeModelSupport class described at
       *   http://java.sun.com/products/jfc/tsc/articles/jtree/index.html
       * That architecture (produced by Tom Santos and Steve Wilson)
       * is more elegant. I just hacked 'em in here so they are
       * immediately at hand.
       */
/*    public void fireTreeNodesChanged( TreeModelEvent e ) {
        Enumeration listeners = listenerList.elements();
        while ( listeners.hasMoreElements() ) {
            TreeModelListener listener =
            (TreeModelListener) listeners.nextElement();
            listener.treeNodesChanged( e );
        }
    }
    public void fireTreeNodesInserted( TreeModelEvent e ) {
        Enumeration listeners = listenerList.elements();
        while ( listeners.hasMoreElements() ) {
            TreeModelListener listener =
            (TreeModelListener) listeners.nextElement();
            listener.treeNodesInserted( e );
        }
    }
    public void fireTreeNodesRemoved( TreeModelEvent e ) {
        Enumeration listeners = listenerList.elements();
        while ( listeners.hasMoreElements() ) {
            TreeModelListener listener =
            (TreeModelListener) listeners.nextElement();
            listener.treeNodesRemoved( e );
        }
    }
    public void fireTreeStructureChanged( TreeModelEvent e ) {
        Enumeration listeners = listenerList.elements();
        while ( listeners.hasMoreElements() ) {
            TreeModelListener listener =
            (TreeModelListener) listeners.nextElement();
            listener.treeStructureChanged( e );
        }
    } */
}
