package ModelInterface.ModelGUI2;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.DOMException;
import org.w3c.dom.ls.*;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;

import javax.swing.TransferHandler;
import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.tree.TreePath;

import java.util.ArrayList;

import java.io.IOException;

import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.QueryTreeModel;
import ModelInterface.ModelGUI2.QueryTreeModel.QueryGroup;

public class QueryTransferHandler extends TransferHandler {
	private Document doc;
	private DOMImplementationLS implls;
	private LSParser parser;

	// should be final but compilier won't let me..
	private static DataFlavor localQueryGeneratorFlavor;
	private static DataFlavor localQueryGroupFlavor;
	private static DataFlavor localNodeFlavor;
	private static final DataFlavor serialQueryGeneratorFlavor = new DataFlavor(QueryGenerator.class, "QueryGenerator");
	private static final DataFlavor serialQueryGroupFlavor = new DataFlavor(QueryGroup.class, "QueryGroup");
	private static final DataFlavor serialNodeFlavor = new DataFlavor(QueryGroup.class, "QueryGroup");
	private static final String localQueryGeneratorType = DataFlavor.javaJVMLocalObjectMimeType+
		";class=ModelInterface.ModelGUI2.queries.QueryGenerator";
	private static final String localQueryGroupType = DataFlavor.javaJVMLocalObjectMimeType+
		";class=ModelInterface.ModelGUI2.QueryTreeModel$QueryGroup";
	private static final String localNodeType = DataFlavor.javaJVMLocalObjectMimeType+
		";class=org.w3c.dom.Node";
	private int lastAction;

	static {
		try {
			localQueryGeneratorFlavor = new DataFlavor(localQueryGeneratorType);
			localQueryGroupFlavor = new DataFlavor(localQueryGroupType);
			localNodeFlavor = new DataFlavor(localNodeType);
		} catch(ClassNotFoundException cnf) {
			// some how I don't think I will get one of these
			cnf.printStackTrace();
		}
	}

	public QueryTransferHandler(Document docIn, DOMImplementationLS impllsIn) {
		super();
		if(localQueryGeneratorFlavor == null) {
			// !!! stupid compilier
			localQueryGeneratorFlavor = null;
		}
		if(localQueryGroupFlavor == null) {
			// !!! stupid compilier
			localQueryGroupFlavor = null;
		}	
		doc = docIn;
		implls = impllsIn;
		try {
			parser = implls.createLSParser(DOMImplementationLS.MODE_SYNCHRONOUS, null);
			parser.setFilter(new ParseFilter());
		} catch (DOMException de) {
			de.printStackTrace();
			// warn couldn't create parser ?
		}
	}
	public boolean importData(JComponent comp, Transferable t) {
		DataFlavor[] transFlavors = t.getTransferDataFlavors();
		if(!canImport(comp, transFlavors)) {
			return false;
		}
		QueryGenerator qg = null;
		QueryGroup qGroup = null;
		Node node = null;
		boolean hasLocalGen = false;
		boolean hasSerialGen = false;
		boolean hasLocalGroup = false;
		boolean hasSerialGroup = false;
		boolean hasLocalNode = false;
		boolean hasSerialNode = false;
		boolean hasString = false;
		for(DataFlavor tranFlav : transFlavors) {
			if(localQueryGeneratorFlavor.equals(tranFlav)) {
				hasLocalGen = true;
			} else if(serialQueryGeneratorFlavor.equals(tranFlav)) {
				hasSerialGen = true;
			} else if(localQueryGroupFlavor.equals(tranFlav)) {
				hasLocalGroup = true;
			} else if(serialQueryGroupFlavor.equals(tranFlav)) {
				hasSerialGroup = true;
			} else if(localNodeFlavor.equals(tranFlav)) {
				hasLocalNode = true;
			} else if(serialNodeFlavor.equals(tranFlav)) {
				hasSerialNode = true;
			} else if(tranFlav.isFlavorTextType()) {
				hasString = true;
			}
		}
		try {
			if(hasLocalGen && lastAction == MOVE) {
				System.out.println("Doing local Gen");
				qg = (QueryGenerator)t.getTransferData(localQueryGeneratorFlavor);
			} else if(hasSerialGen) {
				System.out.println("Doing serial Gen");
				qg = (QueryGenerator)t.getTransferData(serialQueryGeneratorFlavor);
			} else if(hasLocalGroup && lastAction == MOVE) {
				System.out.println("Doing local Grp");
				qGroup = (QueryGroup)t.getTransferData(localQueryGroupFlavor);
			} else if(hasSerialGroup) {
				System.out.println("Doing serial Grp");
				qGroup = (QueryGroup)t.getTransferData(serialQueryGroupFlavor);
			} else if(hasLocalNode) {
				System.out.println("Doing local Node");
				// I don't think nodes are serializable..
				node = (Node)t.getTransferData(localNodeFlavor);
			} else if(hasSerialNode) {
				System.out.println("Doing serial Node");
				node = (Node)t.getTransferData(serialNodeFlavor);
			} else if(hasString) {
				System.out.println("Doing String");
				String str = (String)t.getTransferData(DataFlavor.stringFlavor);
				LSInput tempInput = implls.createLSInput();
				tempInput.setStringData(str);
				node = doc.adoptNode(parser.parse(tempInput).getDocumentElement());
			} else {
				System.out.println("Returning false");
				return false;
			}
		} catch(UnsupportedFlavorException ufe) {
			ufe.printStackTrace();
			return false;
		} catch(IOException ioe) {
			ioe.printStackTrace();
			return false;
		}

		JTree target = (JTree)comp;
		TreePath selPath = target.getSelectionPath();
		QueryTreeModel qt = (QueryTreeModel)target.getModel();
		if(node != null) {
			if(node.getNodeName().equals("queryGroup")) {
				qGroup = qt.createQueryGroup(node);
			} else {
				qg = new QueryGenerator(node);
				if(!qg.isValid()) {
					qg = null;
				}
			}
		}
		if(qg != null) {
			qt.add(selPath, qg);
			return true;
		} else if(qGroup != null) {
			if(!qGroup.getName().equals("MultipleQuerySelection")) {
				qt.add(selPath, qGroup);
			} else {
				Object curr;
				ArrayList children = qGroup.getQueryList();
				for(int i = 0; i < children.size(); ++i) {
					curr = children.get(i);
					if(curr instanceof QueryGenerator) {
						qt.add(selPath, (QueryGenerator)curr);
					} else if(curr instanceof QueryGroup) {
						qt.add(selPath, (QueryGroup)curr);
					}
				}
			}
			return true;
		} else {
			return false;
		}
	}
	public boolean canImport(JComponent comp, DataFlavor[] transFlavors) {
		for(DataFlavor tranFlav : transFlavors) {
			if(localQueryGeneratorFlavor.equals(tranFlav) ||
				serialQueryGeneratorFlavor.equals(tranFlav) ||
				localQueryGroupFlavor.equals(tranFlav) ||
				serialQueryGroupFlavor.equals(tranFlav) ||
				localNodeFlavor.equals(tranFlav) ||
				serialNodeFlavor.equals(tranFlav) ||
				tranFlav.isFlavorTextType()) {
				return true;
			}
		}
		return false;
	}
	protected Transferable createTransferable(JComponent comp) {
		return new TransferableQuery((JTree)comp);
	}
	protected void exportDone(JComponent comp, Transferable t, int action) {
		lastAction = action;
		if(action == MOVE) {
			((JTree)comp).removeSelectionPaths(((JTree)comp).getSelectionPaths());
			// do I need to remove from the model, is it going to delete it?
			// don't want it to be deleted do I?
		}
	}
	public int getSourceActions(JComponent c) {
		return COPY_OR_MOVE;
	}

	private class TransferableQuery implements Transferable {
		private JTree qt;
		private DataFlavor[] transFlavors;
		private Object data;
		public TransferableQuery(JTree qt) {
			this.qt = qt;
			TreePath[] paths = qt.getSelectionPaths();
			if(paths.length == 1) {
				data = qt.getSelectionPath().getLastPathComponent();
			} else {
				ArrayList dataGrouped = new ArrayList(paths.length);
				for(TreePath path : paths) {
					dataGrouped.add(path.getLastPathComponent());
				}
				data = ((QueryTreeModel)qt.getModel()).createQueryGroup("MultipleQuerySelection", dataGrouped);
			}
			transFlavors = new DataFlavor[5];
			if(data instanceof QueryGenerator) {
				transFlavors[0] = localQueryGeneratorFlavor;
				transFlavors[1] = serialQueryGeneratorFlavor;
			} else {
				transFlavors[0] = localQueryGroupFlavor;
				transFlavors[1] = serialQueryGroupFlavor;
			}
			transFlavors[2] = DataFlavor.stringFlavor;
			transFlavors[3] = localNodeFlavor;
			transFlavors[4] = serialNodeFlavor;
		}
		public DataFlavor[] getTransferDataFlavors() {
			return transFlavors;
		}
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			for(DataFlavor tranFlav : transFlavors) {
				if(tranFlav.equals(flavor)) {
					return true;
				}
			}
			return false;
		}
		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
			if(!isDataFlavorSupported(flavor)) {
				throw new UnsupportedFlavorException(flavor);
			}
			if(transFlavors[0].equals(flavor) || transFlavors[1].equals(flavor)) {
				return data;
			} else {
				Node toSerialize;
				if(data instanceof QueryGenerator) {
					toSerialize = ((QueryGenerator)data).getAsNode(doc);
				} else {
					toSerialize = doc.createElement("queryGroup");
					System.out.println(toSerialize);
					((Element)toSerialize).setAttribute("name", ((QueryGroup)data).getName());
					System.out.println(toSerialize);
					((QueryTreeModel)qt.getModel()).addQueryGroup(doc, toSerialize, ((QueryGroup)data));
					System.out.println(toSerialize);
				}
				if(flavor.isFlavorTextType()) {
					return implls.createLSSerializer().writeToString(toSerialize);
				} else {
					return toSerialize;
				}
			}
		}
	}
}
