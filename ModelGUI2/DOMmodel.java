//import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.*;
import java.util.Vector;
import java.util.*;

import org.w3c.dom.*;

public class DOMmodel implements TreeModel {
	private Vector treeModelListeners = new Vector();
	private Node rootNode;

	public DOMmodel(Node root) {
        	rootNode = root;
 		treeModelListeners.clear();
	}

	public Object getRoot() {
		return new DOMNodeAdapter(rootNode);
	}

	public int getChildCount(Object parent) {
		return ((DOMNodeAdapter)parent).getNode().getChildNodes().getLength();
		/*NodeList nl = ((DOMNodeAdapter)parent).getNode().getChildNodes();
		int temp = 0;
		for (int i =0; i < nl.getLength(); i++) {
			if (nl.item(i).getNodeType() != Node.TEXT_NODE) {
				temp++;
			}
		}
		return temp;
		*/
	}

	public Object getChild(Object parent, int index) {
		return new DOMNodeAdapter (((DOMNodeAdapter)parent).getNode().getChildNodes().item(index));
	}

	public boolean isLeaf(Object node) {
		return !( ((DOMNodeAdapter)node).getNode().hasChildNodes() );
	}

	// returns -1 if parent or child is null and also if child isn't one of
	// parent's children.
	public int getIndexOfChild(Object parent, Object child) {
		if(parent == null || child == null){
			return -1;
		}
		else{
			/*
			if (((DOMNodeAdapter)child).getNode().getNodeType() == Node.TEXT_NODE) {
				System.out.println("HERE");
				return -1;
			}
			*/
			try {
			int counter = 0;
			NodeList childlist = ((DOMNodeAdapter)parent).getNode().getChildNodes();
			while(counter < childlist.getLength()){
				if (childlist.item(counter).getNodeType() == Node.TEXT_NODE
					  && ((DOMNodeAdapter)child).getNode().getNodeType() == Node.TEXT_NODE) {
					if (childlist.item(counter).getNodeValue().equals(((DOMNodeAdapter)child).getNode().getNodeValue())) {
						return counter;
					}
				}
				else if (((DOMNodeAdapter)child).getNode().getNodeType() != Node.TEXT_NODE && DOMTreeBuilder.compareHelper( ((Element)childlist.item(counter)),
					((Element)((DOMNodeAdapter)child).getNode() ))){
					return counter;
				}
				counter++;
			}
			} catch (ClassCastException e) {
				System.out.println(e);
				return -1;
			}
			return -1;
		}
	}


    public void addTreeModelListener(TreeModelListener l) {
	    if (l != null && !treeModelListeners.contains(l)) {
        	treeModelListeners.addElement(l);
	    }
    }

	public void removeTreeModelListener(TreeModelListener l) {
		if (l != null) {
			treeModelListeners.removeElement(l);
		}
	}

    public void valueForPathChanged(TreePath path, Object newValue) {
        System.out.println("ValueForPathChanged, path: " + path + ", newValue: " + newValue);
        Object oldValue = path.getLastPathComponent();
		//if the value of the node has not changed, do nothing
		if(oldValue.equals(newValue)) return;
        
        
        Node child = ((DOMmodel.DOMNodeAdapter)path.getLastPathComponent()).getNode();
        if( child.getNodeType() == Element.TEXT_NODE ){
        	//child.setNodeValue( (String)newValue );
		child.setNodeValue( newValue.toString() );
        }else{
        	System.out.println("ERROR: can only change text nodes!");
        }
        
	TreeModelListener listener;
	Iterator it = treeModelListeners.iterator();
	while (it.hasNext()) {
		listener = (TreeModelListener)it.next();
		//listener.treeNodesChanged(new TreeModelEvent(this, path.getParentPath()));
		listener.treeNodesChanged(new TreeModelEvent(this, path));
	}
	return; 
  	//api says: Messaged when the user has altered the value for the item
  	//identified by path to newValue. If newValue signifies a truly new value
  	//the model should post a treeNodesChanged event.
    }

    public DOMNodeAdapter getAdapterNode( Node e ) {
	    return new DOMNodeAdapter(e);
    }

    public class DOMNodeAdapter {
		Node n;
		public DOMNodeAdapter( Node e) {
			n = e;
		}
		public String toString() {
			if (n.getNodeType() == Node.TEXT_NODE) {
				return n.getNodeValue();
			}
			String ret;
			ret = n.getNodeName();
			NamedNodeMap attrs = n.getAttributes();
			for (int i =0; i < attrs.getLength(); i++) {
				ret = ret+" "+attrs.item(i).getNodeName() + " = " + ((Element)n).getAttribute(attrs.item(i).getNodeName());
			}
			return ret;
		}
		public Node getNode() {
			return n;
		}
	}
	
	public void insertNodeInto(Node newChild, TreePath parentPath) {
		//Node parent = (Node)(parentPath.getLastPathComponent());
		Node parent = ((DOMmodel.DOMNodeAdapter)parentPath.getLastPathComponent()).getNode();
		if(parent == null){
			parent = rootNode;
		}
		
		// if equal, don't add ************************************************
		
		parent.appendChild(newChild);
		
		int ct = 0;
		   NodeList children = parent.getChildNodes();
		   Node kid;
		   while(ct < children.getLength()) {
			   kid = children.item(ct);
			   if (kid.equals(newChild)) break;
			   ct++;
		   }
		
	    //int[] arr1 = {getIndexOfChild(parent, newChild)};
	    int[] arr1 = {ct};
		Node[] arr2 = {newChild};
		fireTreeNodesInserted(new TreeModelEvent(this, parentPath, arr1, arr2));
		System.out.println("outputting tree!!!");
	}
	
	public void removeNodeFrom(TreePath nodePath) {
		Node child = ((DOMmodel.DOMNodeAdapter)nodePath.getLastPathComponent()).getNode();
		Node parent = ((Node)child.getParentNode());
        
		int ct = 0;
		   NodeList children = parent.getChildNodes();
		   Node kid;
		   while(ct < children.getLength()) {
			   kid = children.item(ct);
			   if (kid.equals(child)) break;
			   ct++;
		   }
        
		   parent.removeChild(child);
        
		  int[] arr1 = {ct};
		  Node[] arr2 = {child};
		  fireTreeNodesRemoved(new TreeModelEvent(this, nodePath.getParentPath(), arr1, arr2));
	}
	
	public void fireTreeNodesInserted( TreeModelEvent e ) {
		Iterator listeners = treeModelListeners.iterator();
		while ( listeners.hasNext() ) {
			TreeModelListener listener = (TreeModelListener) listeners.next();
			listener.treeNodesInserted( e );
		}
	}
	
	public void fireTreeNodesRemoved( TreeModelEvent e ) {
		Iterator listeners = treeModelListeners.iterator();
		while ( listeners.hasNext() ) {
			TreeModelListener listener = (TreeModelListener) listeners.next();
			listener.treeNodesRemoved( e );
		}
	}

	public void fireTreeNodesChanged( TreeModelEvent e ) {
		Iterator listeners = treeModelListeners.iterator();
		while ( listeners.hasNext() ) {
			TreeModelListener listener = (TreeModelListener) listeners.next();
			listener.treeNodesChanged( e );
		}
	}

}
