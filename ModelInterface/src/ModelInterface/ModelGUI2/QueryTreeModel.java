package ModelInterface.ModelGUI2;

import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.undo.QueryAddRemoveUndoableEdit;

import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeModelEvent;

import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class QueryTreeModel implements TreeModel {
	protected QueryGroup root;
	protected ArrayList<TreeModelListener> tmListeners;
	public QueryTreeModel(Node n) {
		root = new QueryGroup("All", recCreateTree(n));
		tmListeners = new ArrayList<TreeModelListener>();
		addTreeModelListener(new QueryTreeModelListener());
	}
	protected ArrayList recCreateTree(Node n) {
		NodeList nl = n.getChildNodes();
		ArrayList ret = new ArrayList(nl.getLength());
		for(int i = 0; i < nl.getLength(); ++i) {
			if(nl.item(i).getNodeName().equals("queryGroup")) {
				ret.add(new QueryGroup(((Element)nl.item(i)).getAttribute("name"), recCreateTree(nl.item(i))));
			} else {
				ret.add(new QueryGenerator(nl.item(i)));
			}
		}
		return ret;
	}
	public void addTreeModelListener(TreeModelListener l) {
		tmListeners.add(l);
	}
	public Object getChild(Object parent, int index) {
		if(!(parent instanceof QueryGroup) || parent == null) {
			// should throw some exception
			return null;
		}
		return ((QueryGroup)parent).getQueryList().get(index);
	}
	public int getChildCount(Object parent) {
		if(!(parent instanceof QueryGroup) || parent == null) {
			// should throw some exception
			return -1;
		}
		return ((QueryGroup)parent).getQueryList().size();
	}
	public int getIndexOfChild(Object parent, Object child) {
		if(!(parent instanceof QueryGroup) || (!(child instanceof QueryGroup) && !(child instanceof QueryGenerator)) || 
				parent == null || child == null) {
			// should throw some exception
			return -1;
		}
		return ((QueryGroup)parent).getQueryList().indexOf(child);
	}
	public Object getRoot() {
		return root;
	}
	public boolean isLeaf(Object node) {
		if(node == null) {
			return false;
		} else if(node instanceof QueryGenerator) {
			return true;
		} else if(node instanceof QueryGroup) {
			return false;
		} else {
			// error?
			return false;
		}
	}
	public void removeTreeModelListener(TreeModelListener l) {
		tmListeners.remove(l);
	}
	public void valueForPathChanged(TreePath path, Object newValue) {
		// error? the user should not be alowed to change values in the tree directly
	}
	public void add(TreePath path, QueryGenerator qg) {
		addValue(path, qg);
	}
	public void add(TreePath path, String gN) {
		addValue(path, new QueryGroup(gN, new ArrayList()));
	}
	private void addValue(TreePath path, Object val) {
		TreePath parentPath;
		if(path.getLastPathComponent() instanceof QueryGroup) {
			parentPath = path;
		} else {
			parentPath = path.getParentPath();
		}
		QueryGroup parent = (QueryGroup)parentPath.getLastPathComponent();
		int[] index = new int[1];
		Object[] children = new Object[1];
		children[0] = val;
		index[0] = parent.getQueryList().size();
		parent.getQueryList().add(val);
		fireTreeNodesInserted(this, parentPath, index, children);
	}
	public void remove(TreePath path) {
		Object[] lastComp = new Object[1];
		int[] index;
		lastComp[0] = path.getLastPathComponent();
		if(lastComp[0] instanceof QueryGroup) {
			// ask sure to remove all of this group
		}
		if(lastComp[0].equals(root)) {
			Object[] children = root.getQueryList().toArray();
			index = new int[children.length];
			for(int i = 0; i < index.length; ++i) {
				index[i] = 1;
			}
			root.getQueryList().clear();
			fireTreeNodesRemoved(this, path, index, children);
		} else {
			index = new int[1];
			index[0] = ((QueryGroup)path.getParentPath().getLastPathComponent()).getQueryList().indexOf(lastComp[0]);
			((QueryGroup)path.getParentPath().getLastPathComponent()).getQueryList().remove(index[0]);
			fireTreeNodesRemoved(this, path.getParentPath(), index, lastComp);
		}
	}
	public Node getAsNode(Document doc) {
		addQueryGroup(doc, doc.getDocumentElement(), root);
		return doc.getDocumentElement();
	}
	public void addQueryGroup(Document doc, Node currNode, QueryGroup currGroup) {
		Node tempNode;
		for(int i = 0; i < currGroup.getQueryList().size(); ++i) {
			if(currGroup.getQueryList().get(i) instanceof QueryGroup) {
				tempNode = doc.createElement("queryGroup");
				((Element)tempNode).setAttribute("name", ((QueryGroup)currGroup.getQueryList().get(i)).getName());
				addQueryGroup(doc, tempNode, (QueryGroup)currGroup.getQueryList().get(i));
			} else {
				tempNode = ((QueryGenerator)currGroup.getQueryList().get(i)).getAsNode(doc);
			}
			currNode.appendChild(tempNode);
		}
	}
	public void fireTreeNodesChanged(Object source, TreePath path, int[] childIndices, Object[] children) {
		TreeModelEvent e = new TreeModelEvent(source, path, childIndices, children);
		for(Iterator<TreeModelListener> it = tmListeners.iterator(); it.hasNext(); ) {
			it.next().treeNodesChanged(e);
		}
	}
	public void fireTreeNodesInserted(Object source, TreePath path, int[] childIndices, Object[] children) {
		TreeModelEvent e = new TreeModelEvent(source, path, childIndices, children);
		for(Iterator<TreeModelListener> it = tmListeners.iterator(); it.hasNext(); ) {
			it.next().treeNodesInserted(e);
		}
	}
	public void fireTreeNodesRemoved(Object source, TreePath path, int[] childIndices, Object[] children) {
		TreeModelEvent e = new TreeModelEvent(source, path, childIndices, children);
		for(Iterator<TreeModelListener> it = tmListeners.iterator(); it.hasNext(); ) {
			it.next().treeNodesRemoved(e);
		}
	}
	public void fireTreeStructureChanged(Object source, TreePath path, int[] childIndices, Object[] children) {
		TreeModelEvent e = new TreeModelEvent(source, path, childIndices, children);
		for(Iterator<TreeModelListener> it = tmListeners.iterator(); it.hasNext(); ) {
			it.next().treeStructureChanged(e);
		}
	}
	public class QueryGroup {
		ArrayList qList;
		String groupName;
		public QueryGroup(String name, ArrayList list) {
			groupName = name;
			qList = list;
		}
		public String getName() {
			return groupName;
		}
		public ArrayList getQueryList() {
			return qList;
		}
		public String toString() {
			return groupName;
		}
	}
	protected class QueryTreeModelListener implements TreeModelListener {
		private UndoManager undoManager;
		private InterfaceMain main;
		public QueryTreeModelListener() {
			main = InterfaceMain.getInstance();
			undoManager = main.getUndoManager();
		}
		public void treeNodesChanged(TreeModelEvent e) {
			// don't care about this
		}
		public void treeNodesInserted(TreeModelEvent e) {
			if(e.getSource() instanceof QueryTreeModel) {
				addEdit(new QueryAddRemoveUndoableEdit(e, true));
			}
		}
		public void treeNodesRemoved(TreeModelEvent e) {
			if(e.getSource() instanceof QueryTreeModel) {
				addEdit(new QueryAddRemoveUndoableEdit(e, false));
			}
		}
		public void treeStructureChanged(TreeModelEvent e) {
			// don't care about this
		}
		private void addEdit(UndoableEdit e) {
			undoManager.addEdit(e);
			main.refreshUndoRedo();
		}
	}
}

