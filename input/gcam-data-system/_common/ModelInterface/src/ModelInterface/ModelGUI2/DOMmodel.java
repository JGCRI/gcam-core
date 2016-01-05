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
package ModelInterface.ModelGUI2;

import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.*;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import java.util.Vector;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import org.w3c.dom.*;
import org.w3c.dom.events.MutationEvent;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventTarget;
import org.w3c.dom.events.EventListener;

import ModelInterface.ModelGUI2.undo.NodeInsertUndoableEdit;
import ModelInterface.ModelGUI2.undo.NodeDeleteUndoableEdit;
import ModelInterface.ModelGUI2.undo.NodeEditUndoableEdit;
import ModelInterface.InterfaceMain;

public class DOMmodel implements TreeModel {
	private Vector treeModelListeners = new Vector();
	private Node rootNode;
	private Document doc;

	/**
	 * Initializes the root of the tree, and adds listeners to the
	 * Document to be able to tell the JTree when the Document has
	 * been modified so that it can update itself.
	 * @param doc Document node which will be used to get the root of the tree
	 */
	public DOMmodel(Document doc) {
		this.doc = doc;
        	rootNode = doc.getDocumentElement();
        final InterfaceMain main = InterfaceMain.getInstance();
 		treeModelListeners.clear();
		final EventTarget target = (EventTarget)doc;
		target.addEventListener("DOMNodeInserted", new EventListener() {
			public void handleEvent(Event aEvent) {
				System.out.println("HANDLIN EVENT, type: "+aEvent.getType());
				MutationEvent event = (MutationEvent)aEvent;
				final DOMNodeAdapter target  = new DOMNodeAdapter((Node)event.getTarget());
				final Node rel = event.getRelatedNode();
				int insPos = getIndexOfChild(new DOMNodeAdapter(rel), target);
				int[] posArr = {insPos};
				Object[] childArr = {target};
				TreeModelEvent tEvent = new TreeModelEvent(this, getTreePathFromNode(rel), posArr, childArr);
				// do I have to make sure this event isn't from an undo/redo
				// and if it is, do I not create a new edit?
				// too tell if this change can from an Undo/redo(in which case I don't create a new edit)
				// I set user data on the node under the key "isFromUndoable"
				if(new Boolean(true).equals(target.getNode().getUserData("isFromUndoable"))) {
					System.out.println("Thinks this is from undo");
					// set to false or null?
					target.getNode().setUserData("isFromUndoable", false, null);
				} else if (target.getNode().getUserData("isSetValue") != null) {
					System.out.println("Putting as compound edit");
					System.out.println("Add to CompEdit returned: "+((UndoableEdit)target.getNode().getUserData("isSetValue")).addEdit(
								   new NodeInsertUndoableEdit(rel, target.getNode())));
					target.getNode().setUserData("isSetValue", null, null);
				} else {
					System.out.println("Thinks this is your regular or insert");
					main.getUndoManager().addEdit(new NodeInsertUndoableEdit(rel, target.getNode()));
				}
				main.refreshUndoRedo();
				fireTreeNodesInserted(tEvent);
			}
		}, false);
		target.addEventListener("DOMNodeRemoved", new EventListener() {
			public void handleEvent(Event aEvent) {
				System.out.println("HANDLIN EVENT, type: "+aEvent.getType());
				MutationEvent event = (MutationEvent)aEvent;
				final DOMNodeAdapter target  = new DOMNodeAdapter((Node)event.getTarget());
				final Node rel = event.getRelatedNode();
				int insPos = getIndexOfChild(new DOMNodeAdapter(rel), target);
				int[] posArr = {insPos};
				Object[] childArr = {target};
				TreeModelEvent tEvent = new TreeModelEvent(this, getTreePathFromNode(rel), posArr, childArr);
				// do I have to make sure this event isn't from an undo/redo
				// and if it is, do I not create a new edit?
				if(!new Boolean(true).equals(target.getNode().getUserData("isFromUndoable"))) {
					main.getUndoManager().addEdit(new NodeDeleteUndoableEdit(rel, target.getNode()));
				} else {
					target.getNode().setUserData("isFromUndoable", false, null);
				}
				main.refreshUndoRedo();
				fireTreeNodesRemoved(tEvent);
			}
		}, false);
	}

	/**
	 * Returns the rootNode inside the wrapper class
	 * @return DOM node adaped rootNode
	 */ 
	public Object getRoot() {
		return new DOMNodeAdapter(rootNode);
	}

	/**
	 * Gets the number of child for a node in the tree.
	 * @param parent node in the tree which the number of children is needed
	 * @return the number of children of the node passed in
	 */
	public int getChildCount(Object parent) {
		//return ((DOMNodeAdapter)parent).getNode().getChildNodes().getLength();
		NodeList nl = ((DOMNodeAdapter)parent).getNode().getChildNodes();
		int temp = 0;
		for (int i =0; i < nl.getLength(); ++i) {
			if (nl.item(i).getNodeType() != Node.COMMENT_NODE) {
				++temp;
			}
		}
		return temp;
	}

	/**
	 * Return the child specified by an index of a certain node
	 * @param parent parent node of the node requested
	 * @param index child position of the parent node passed in
	 * @return the child node as requested.
	 */
	public Object getChild(Object parent, int index) {
		Node n = ((DOMNodeAdapter)parent).getNode().getChildNodes().item(index);
		int childCount = ((DOMNodeAdapter)parent).getNode().getChildNodes().getLength();
		while(index < childCount && n.getNodeType() == Node.COMMENT_NODE) {
			n = ((DOMNodeAdapter)parent).getNode().getChildNodes().item(++index);
		}
		return new DOMNodeAdapter(n);
		//return new DOMNodeAdapter (((DOMNodeAdapter)parent).getNode().getChildNodes().item(index));
	}

	/**
	 * Determine if this node has any children
	 * @param node node in question
	 * @return true if it doesn't have any children, false otherwise
	 */
	public boolean isLeaf(Object node) {
		return getChildCount(node) == 0;
		//return !( ((DOMNodeAdapter)node).getNode().hasChildNodes() );
	}

	/**
	 * Determines the index of a child node in the set of nodes of a parent.
	 * @param parent the parent of the child
	 * @param child the child we want the index of
	 * @return -1 if parent or child is null and also if child isn't one of parent's children, index otherwise
	 */ 
	public int getIndexOfChild(Object parent, Object child) {
		if(parent == null || child == null){
			return -1;
		}
		else{
			try {
			int counter = 0;
			NodeList childlist = ((DOMNodeAdapter)parent).getNode().getChildNodes();
			while(counter < childlist.getLength()){
				// Skip comment nodes.
				if( childlist.item(counter).getNodeType() == Node.COMMENT_NODE){
					// do nothing.
				}
				else if (childlist.item(counter).getNodeType() == Node.TEXT_NODE
					  && ((DOMNodeAdapter)child).getNode().getNodeType() == Node.TEXT_NODE) {
					if (childlist.item(counter).getNodeValue().equals(((DOMNodeAdapter)child).getNode().getNodeValue())) {
						return counter;
					}
				}
				else if (((DOMNodeAdapter)child).getNode().getNodeType() != Node.TEXT_NODE && 
						((DOMNodeAdapter)child).getNode().getNodeType() == Node.ELEMENT_NODE 
						&& compareHelper( ((Element)childlist.item(counter)),
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
	private boolean compareHelper(Element e1, Element e2){ // helper for compare function
		// first make sure tag names are the same
		if( !((e1.getTagName()).equals(e2.getTagName()) ) ){
            // Not the same
            return false;
		}

		// go through all the attributes, make sure have the same ammount and the have the same values
		NamedNodeMap attrs1 = e1.getAttributes();
		NamedNodeMap attrs2 = e2.getAttributes();
		String temp;
		if (attrs1.getLength() != attrs2.getLength()) {
			return false;
		}
		for (int i = 0; i < attrs1.getLength(); i++) {
			temp = attrs1.item(i).getNodeName();
			if (!(e1.getAttribute(temp).equals(e2.getAttribute(temp)))) {
				return false;
			}
		}
		return true;
	}


    /**
     * Adds listeners so that we can notify them we there is an event.
     * @param l a reference to a class that can handle these notifications
     */
    public void addTreeModelListener(TreeModelListener l) {
	    if (l != null && !treeModelListeners.contains(l)) {
        	treeModelListeners.addElement(l);
	    }
    }

        /**
	 * Removes a listener that was already registered.
     	 * @param l a reference to a class that can handle these notifications
	 */ 
	public void removeTreeModelListener(TreeModelListener l) {
		if (l != null) {
			treeModelListeners.removeElement(l);
		}
	}

    /**
     * Update a value in the tree given a TreePath to the Node that has changed.
     * @param path the path to the node that has changed.
     * @param newValue the new value that the node should be changed to
     */
    public void valueForPathChanged(TreePath path, Object newValue) {
	    valueForPathChanged(path, newValue, true);
    }

    /**
     * Update a value in the tree given a TreePath to the Node that has changed.
     * @param path the path to the node that has changed.
     * @param newValue the new value that the node should be changed to
     * @param createEdit whether or not to create a new Undoable Edit, i.e. did the call come as an undo/redo
     */
    public void valueForPathChanged(TreePath path, Object newValue, boolean createEdit) {
	    DOMNodeAdapter changed = (DOMNodeAdapter)path.getLastPathComponent();
	    String oldValStr = changed.toEditString();
	    if(changed.setFromEditString((String)newValue)) {
		    if(createEdit) {
			    InterfaceMain main = InterfaceMain.getInstance();
			    main.getUndoManager().addEdit(new NodeEditUndoableEdit(this, path, oldValStr, (String)newValue));
			    main.refreshUndoRedo();
		    }
		    TreeModelListener listener;
		    Iterator it = treeModelListeners.iterator();
		    TreeModelEvent event = new TreeModelEvent(this, path);
		    while (it.hasNext()) {
			    listener = (TreeModelListener)it.next();
			    listener.treeNodesChanged(event);
		    }
	    }
    }

    /**
     * Returns an wrapped node given a node.
     * @param e node to be wrapped
     * @return the wrapped version of the given node.
     */
    public DOMNodeAdapter getAdapterNode( Node e ) {
	    return new DOMNodeAdapter(e);
    }

    /**
     * A wrapper class of a DOM node so that we can override the toString and have
     * elements and values show up in a more sensible way.
     */
    public class DOMNodeAdapter {
		Node n;
		/**
		 * Default constructor.
		 * @param e The node to wrap.
		 */
		public DOMNodeAdapter( Node e) {
			n = e;
		}
		/**
		 * Return String version of the Node which is nice for the user to read.
		 * Text nodes just return their value.  Elements return in the form:
		 * NodeName attrName = attrVal..
		 * @return String representation.
		 */
		public String toString() {
			if (n.getNodeType() == Node.TEXT_NODE) {
				return n.getNodeValue();
			}
			String ret;
			ret = n.getNodeName();
			NamedNodeMap attrs = n.getAttributes();
			// Node may not be an element so we need to check
			// if it has attributes.
			if(attrs != null){
				for (int i =0; i < attrs.getLength(); i++) {
					ret = ret+" "+attrs.item(i).getNodeName() + " = " + ((Element)n).getAttribute(attrs.item(i).getNodeName());
				}
			}
			return ret;
		}
		/**
		 * Gets the wrapped node.
		 * @return The wrapped Node.
		 */
		public Node getNode() {
			return n;
		}
		/**
		 * Determines whether these DOMNodeAdapters are equal.  They
		 * are the same when the node that they wrap are the same.
		 * @return True if they are equal, false otherwise.
		 */
		public boolean equals(Object o) {
			if(!(o instanceof DOMNodeAdapter)) {
				return false;
			}
			return n.equals(((DOMNodeAdapter)o).getNode());
		}
		/**
		 * Gets a hashcode.  Returns the wrapped Node's hasCode so that
		 * the equals/hashcode contract thing is fulfilled.
		 * @return A hashcode.
		 */
		public int hashCode() {
			return n.hashCode();
		}
		/**
		 * Returns a string representation of the wrapped node that the user is
		 * presented with when editing the node.  This version removes some ambiguity
		 * that the toString version has which makes it easier to validate. For Elements
		 * the format looks like: nodeName[, attrName = "attrValue"]*.  For Text nodes
		 * it is just he node value.
		 * @return A String representation of the Node which is suitable for editing.
		 */
		public String toEditString() {
			StringBuilder val = new StringBuilder();
			if(n.getNodeType() == Node.TEXT_NODE) {
				val.append(n.getNodeValue());
			} else if(n.getNodeType() == Node.ELEMENT_NODE) {
				val.append(n.getNodeName());
				NamedNodeMap nnm = n.getAttributes();
				for(int i = 0; i < nnm.getLength(); ++i) {
					Node currItem = nnm.item(i);
					val.append(", ").append(currItem.getNodeName())
						.append(" = \"").append(currItem.getNodeValue()).append('"');
				}
			}
			return val.toString();
		}
		/**
		 * Sets the prooperties of the wrapped node from a String in the format of 
		 * toEditString.  For text nodes the node value is set the newValue.  For
		 * element nodes the nodeName, and attrubutes are extracted from newValue. 
		 * The element may then be renamed, and may have attributes added, removed,
		 * or set to a new value.
		 * @param newValue String which specifies the new properties for the Node that
		 * 			is being wrapped.
		 * @return True if the node acctually changed, false otherwise.
		 */
		public boolean setFromEditString(String newValue) {
			boolean didChange = false;
			if( n.getNodeType() == Element.TEXT_NODE && !n.getNodeValue().equals(newValue)) {
				n.setNodeValue( newValue );
				didChange = true;
			} else if(n.getNodeType() == Node.ELEMENT_NODE) {
				Pattern regex = Pattern.compile("(?:\\s*([\\w\\-_]+)\\s*)?(?:,\\s*([\\w\\-_]+)\\s*=\\s*\"([^\"]+)\"\\s*)");
				Matcher matcher = regex.matcher(newValue);
				String temp1;
				Map<String, String> newAttrs = new HashMap<String, String>();
				if(!matcher.find()) {
					// must be just the node name..
					temp1 = newValue;
				} else {
					temp1 = matcher.group(1);
					do {
						newAttrs.put(matcher.group(2), matcher.group(3));
					} while(matcher.find());
				}
				if(!n.getNodeName().equals(temp1)) {
					doc.renameNode(n, "", temp1);
					didChange = true;
				}
				NamedNodeMap nnm = n.getAttributes();
				for(int i = 0; i < nnm.getLength(); ++i) {
					Node currItem = nnm.item(i);
					if(newAttrs.containsKey(currItem.getNodeName())) {
						if(!currItem.getNodeValue().equals(newAttrs.get(currItem.getNodeName()))) {
							currItem.setNodeValue(newAttrs.get(currItem.getNodeName()));
							didChange = true;
						}
						newAttrs.remove(currItem.getNodeName());
					} else {
						nnm.removeNamedItem(currItem.getNodeName());
						--i; // really want an iterator here..
						didChange = true;

					}
				}
				for(Iterator<Map.Entry<String, String>> it = newAttrs.entrySet().iterator(); it.hasNext(); ) {
					Map.Entry<String, String> curr = it.next();
					((Element)n).setAttribute(curr.getKey(), curr.getValue());
					didChange = true;
				}
			}
			return didChange;
		}
	}
	
        /**
	 * Used to insert a new node into the tree given the path where it should go
	 * @param newChild the new node to be added to the tree.
	 * @param parentPath the path to where the node is to be added.
	 */
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
		//System.out.println("outputting tree!!!");
	}
	
	/**
	 * Removes a node from the tree given it's path in the tree.
	 * @param nodePath the path to the node to be removed.
	 */
	public void removeNodeFrom(TreePath nodePath) {
		Node child = ((DOMmodel.DOMNodeAdapter)nodePath.getLastPathComponent()).getNode();
		Node parent = ((Node)child.getParentNode());

		// what if we are removing the parent node
		// doesn't work, not sure what we would do for this.. 
		if(child == rootNode) {
			rootNode = null;
			return;
		}
        
		int ct = 0;
		   NodeList children = parent.getChildNodes();
		   Node kid;
		   while(ct < children.getLength()) {
			   kid = children.item(ct);
			   if (kid.equals(child)) break;
			   ct++;
		   }
        
		   parent.removeChild(child);
		   /*
		   System.out.println("The nodePath is: "+nodePath.getParentPath());
		   TreePath usePath = getTreePathFromNode(parent);
		   System.out.println("Are they the same? "+usePath.equals(nodePath.getParentPath()));
		   System.out.println("HashCodes(good : bad): "+nodePath.getParentPath().hashCode()+" : "+usePath.hashCode());
		   System.out.println("HashCodes(good : bad): "+nodePath.getParentPath().getLastPathComponent().hashCode()+" : "+usePath.getLastPathComponent().hashCode());
        
		  int[] arr1 = {ct};
		  Node[] arr2 = {child};
		  fireTreeNodesRemoved(new TreeModelEvent(this, nodePath.getParentPath(), arr1, arr2));
		  //fireTreeNodesRemoved(new TreeModelEvent(this, nodePath.getParentPath()));
		  */
	}
	
	/**
	 * Go throught the registered listeners and send them the event.
	 * @param e the even that should be sent out
	 */
	public void fireTreeNodesInserted( TreeModelEvent e ) {
		Iterator listeners = treeModelListeners.iterator();
		while ( listeners.hasNext() ) {
			TreeModelListener listener = (TreeModelListener) listeners.next();
			listener.treeNodesInserted( e );
		}
	}
	
	/**
	 * Go throught the registered listeners and send them the event.
	 * @param e the even that should be sent out
	 */
	public void fireTreeNodesRemoved( TreeModelEvent e ) {
		Iterator listeners = treeModelListeners.iterator();
		while ( listeners.hasNext() ) {
			TreeModelListener listener = (TreeModelListener) listeners.next();
			listener.treeNodesRemoved( e );
		}
	}

	/**
	 * Go throught the registered listeners and send them the event.
	 * @param e the even that should be sent out
	 */
	public void fireTreeNodesChanged( TreeModelEvent e ) {
		Iterator listeners = treeModelListeners.iterator();
		while ( listeners.hasNext() ) {
			TreeModelListener listener = (TreeModelListener) listeners.next();
			listener.treeNodesChanged( e );
		}
	}

	public TreePath getTreePathFromNode(Node n) {
		List<DOMNodeAdapter> path = new LinkedList();
		path.add(new DOMNodeAdapter(n));
		while((n = n.getParentNode()) != null) {
			path.add(0, new DOMNodeAdapter(n));
		}
		path.remove(0);
		return new TreePath(path.toArray());
	}

}
