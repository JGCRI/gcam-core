package ModelInterface.ModelGUI2;

import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.queries.SingleQueryExtension;
import ModelInterface.ModelGUI2.undo.MiUndoableEditListener;
import ModelInterface.ModelGUI2.undo.QueryAddRemoveUndoableEdit;
import ModelInterface.ModelGUI2.undo.EditQueryUndoableEdit;

import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeModelEvent;

import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;
import javax.swing.event.UndoableEditEvent;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

public class QueryTreeModel implements TreeModel, MiUndoableEditListener {
	protected QueryGroup root;
	protected ArrayList<TreeModelListener> tmListeners;
	protected int changes;
	public QueryTreeModel(Node n) {
		root = new QueryGroup("queries", recCreateTree(n));
		changes = 0;
		tmListeners = new ArrayList<TreeModelListener>();
		addTreeModelListener(new QueryTreeModelListener());
	}
	protected ArrayList recCreateTree(Node n) {
		NodeList nl = n.getChildNodes();
		ArrayList ret = new ArrayList(nl.getLength());
		QueryGenerator qg;
		for(int i = 0; i < nl.getLength(); ++i) {
			if(nl.item(i).getNodeName().equals("queryGroup")) {
				ret.add(new QueryGroup(((Element)nl.item(i)).getAttribute("name"), recCreateTree(nl.item(i))));
			} else {
				qg = new QueryGenerator(nl.item(i));
				if(qg.isValid()) {
					ret.add(qg);
				}
			}
		}
		return ret;
	}
	public void addTreeModelListener(TreeModelListener l) {
		tmListeners.add(l);
	}
	public Object getChild(Object parent, int index) {
		if(getChildCount(parent) <= 0) {
			return null;
		}
		if(parent instanceof QueryGroup) {
			return ((QueryGroup)parent).getQueryList().get(index);
		} else {
			return ((QueryGenerator)parent).getSingleQueryExtension()
				.getSingleQueryValueAt(index);
		}
	}
	public int getChildCount(Object parent) {
		if((!(parent instanceof QueryGroup) && !(parent instanceof QueryGenerator))
				|| parent == null) {
			// should throw some exception
			return -1;
		}
		if(parent instanceof QueryGroup) {
			return ((QueryGroup)parent).getQueryList().size();
		} else {
			return ((QueryGenerator)parent).getSingleQueryExtension()
				.getNumValues();
		}
	}
	public int getIndexOfChild(Object parent, Object child) {
		if((!(parent instanceof QueryGroup) && !(parent instanceof QueryGenerator)) || 
				(!(child instanceof QueryGroup) && !(child instanceof QueryGenerator) && 
				!(child instanceof SingleQueryExtension.SingleQueryValue)) ||
				 parent == null || child == null) {
			// should throw some exception
			return -1;
		}
		if(isLeaf(parent)) {
			return -1;
		}
		if(parent instanceof QueryGroup) {
			return ((QueryGroup)parent).getQueryList().indexOf(child);
		} else {
			return ((QueryGenerator)parent).getSingleQueryExtension()
				.getIndexOfValue((SingleQueryExtension.SingleQueryValue)child);
		}
	}
	public Object getRoot() {
		return root;
	}
	public boolean isLeaf(Object node) {
		if(node == null) {
			return false;
		} else if(node instanceof QueryGenerator) {
			if(!((QueryGenerator)node).hasSingleQueryExtension()){
			       return true;
			} else {
				return ((QueryGenerator)node).getSingleQueryExtension()
					.getNumValues() == 0;
			}
		} else if(node instanceof QueryGroup) {
			return false;
		} else {
			// single queries
			return true;
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
	public void add(TreePath path, QueryGroup qGroup) {
		addValue(path, qGroup);
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
	public void doEdit(final TreePath editPath) {
		final Object[] children = { editPath.getLastPathComponent() };
		if(!(children[0] instanceof QueryGenerator)) {
			// TODO: support editing query groups
			return;
		}
		final QueryTreeModel thisListener = this;
		Thread editThread = new Thread(new Runnable() {
			public void run() {
				if(((QueryGenerator)children[0]).editDialog(thisListener)) {
					TreePath parentPath = editPath.getParentPath();
					int[] childInds = { getIndexOfChild(parentPath.getLastPathComponent(),
						children[0]) };
					fireTreeNodesChanged(thisListener, parentPath, childInds, children);
				}
				// else no changes, do nothing
			}
		});
		editThread.start();
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
	public void showSingleQuery(SingleQueryExtension se, TreePath parentPath) {
		int numValues = se.getNumValues();
		int[] childIndices = new int[numValues];
		Object[] children = new Object[numValues];
		for(int i = 0; i < numValues; ++i) {
			childIndices[i] = i;
			children[i] = se.getSingleQueryValueAt(i);
		}
		fireTreeNodesInserted(se, parentPath, childIndices, children);
	}
	public void hideSingleQuery(SingleQueryExtension se, TreePath parentPath) {
		int numValues = se.getNumValues();
		int[] childIndices = new int[numValues];
		Object[] children = new Object[numValues];
		for(int i = 0; i < numValues; ++i) {
			childIndices[i] = i;
			children[i] = se.getSingleQueryValueAt(i);
		}
		// let se know if can reset it's list..
		se.resetList();
		fireTreeNodesRemoved(se, parentPath, childIndices, children);
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
	public QueryGroup createQueryGroup(String name, ArrayList list) {
		return new QueryGroup(name, list);
	}
	public QueryGroup createQueryGroup(Node n) {
		// TODO: put in more checks to make sure this node represents
		// a real QueryGroup
		String name = ((Element)n).getAttribute("name");
		if(name == null) {
			return null;
		}
		ArrayList temp = recCreateTree(n);
		if(temp.size() == 0) {
			return null;
		}
		return new QueryGroup(name, temp);
	}
	public class QueryGroup implements java.io.Serializable {
		ArrayList qList;
		String groupName;
		public QueryGroup(String name, ArrayList list) {
			groupName = name;
			qList = list;
		}
		public String getName() {
			return groupName;
		}
		public void setName(String nameIn) {
			groupName = nameIn;
		}
		public ArrayList getQueryList() {
			return qList;
		}
		public String toString() {
			return groupName;
		}
		public void resetQueryBuilders() {
			for(int i = 0; i < qList.size(); ++i) {
				if(qList.get(i) instanceof QueryGenerator) {
					((QueryGenerator)qList.get(i)).resetQueryBuilder();
				} else {
					((QueryGroup)qList.get(i)).resetQueryBuilders();
				}
			}
		}
		private void writeObject(ObjectOutputStream out) throws IOException {
			out.writeObject(groupName);
			out.writeObject(qList);
		}
		private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
			groupName = (String)in.readObject();
			qList = (ArrayList)in.readObject();
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
			if(e.getSource() instanceof QueryTreeModel) {
				increaseChanges();
			}
		}
		public void treeNodesInserted(TreeModelEvent e) {
			if(e.getSource() instanceof QueryTreeModel) {
				increaseChanges();
				addEdit(new QueryAddRemoveUndoableEdit(e, true));
			}
		}
		public void treeNodesRemoved(TreeModelEvent e) {
			if(e.getSource() instanceof QueryTreeModel) {
				increaseChanges();
				addEdit(new QueryAddRemoveUndoableEdit(e, false));
			}
		}
		public void treeStructureChanged(TreeModelEvent e) {
			// don't care about this
			// can only happen with inserts and deletes 
		}
		private void addEdit(UndoableEdit e) {
			undoManager.addEdit(e);
			main.refreshUndoRedo();
		}
	}
	public void undoPerformed(UndoableEditEvent e) {
		if(e.getEdit() instanceof QueryAddRemoveUndoableEdit || 
				e.getEdit() instanceof EditQueryUndoableEdit) {
			decreaseChanges();
		} 
	}

	public void redoPerformed(UndoableEditEvent e) {
		if(e.getEdit() instanceof QueryAddRemoveUndoableEdit || 
				e.getEdit() instanceof EditQueryUndoableEdit) {
			increaseChanges();
		}
	}	

	private void increaseChanges() {
		// TODO: Worry about threading
		++changes;
		if(changes == 1) {
			root.setName(root.getName()+"*");
			int[] indices = { 0 };
			Object[] children = { root };
			TreePath parentPath = new TreePath(root);//.getParentPath();
			fireTreeNodesChanged(root, parentPath, indices, children);
		}
	}

	private void decreaseChanges() {
		// TODO: Worry about threading
		--changes;
		if(!hasChanges()) {
			root.setName(root.getName()
					.substring(0, root.getName().length()-1));
			int[] indices = { 0 };
			Object[] children = { root };
			TreePath parentPath = new TreePath(root);//.getParentPath();
			fireTreeNodesChanged(root, parentPath, indices, children);
		}
	}

	public boolean hasChanges() {
		// TODO: Worry about threading
		return changes != 0;
	}
}

