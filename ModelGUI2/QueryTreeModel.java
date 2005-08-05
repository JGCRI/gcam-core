package ModelGUI2;
import java.util.ArrayList;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.TreeModelListener;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class QueryTreeModel implements TreeModel {
	protected QueryGroup root;
	protected ArrayList tmListeners;
	public QueryTreeModel(Node n) {
		root = new QueryGroup("All", recCreateTree(n));
		tmListeners = new ArrayList();
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
		// error? the use should not be alowed to change values in the tree directly
	}
	public void add(TreePath path, QueryGenerator qg) {
		QueryGroup parent;
		if(path.getLastPathComponent() instanceof QueryGroup) {
			parent = (QueryGroup)path.getLastPathComponent();
		} else {
			parent = (QueryGroup)path.getParentPath().getLastPathComponent();
		}
		parent.getQueryList().add(qg);
	}
	public void add(TreePath path, String gN) {
		QueryGroup parent;
		if(path.getLastPathComponent() instanceof QueryGroup) {
			parent = (QueryGroup)path.getLastPathComponent();
		} else {
			parent = (QueryGroup)path.getParentPath().getLastPathComponent();
		}
		parent.getQueryList().add(new QueryGroup(gN, new ArrayList()));
	}
	public void remove(TreePath path) {
		if(path.getLastPathComponent() instanceof QueryGroup) {
			// ask sure to remove all of this group
		}
		if(path.getLastPathComponent().equals(root)) {
			root.getQueryList().clear();
		} else {
			((QueryGroup)path.getParentPath().getLastPathComponent()).getQueryList().remove(path.getLastPathComponent());
		}
	}
	public Node getAsNode(Document doc) {
		Node ret = doc.createElement("queries");
		addQueryGroup(doc, ret, root);
		return ret;
	}
	protected void addQueryGroup(Document doc, Node currNode, QueryGroup currGroup) {
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
	protected class QueryGroup {
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
}

