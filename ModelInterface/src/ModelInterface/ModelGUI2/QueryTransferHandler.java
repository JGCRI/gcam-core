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
		if(canImport(comp, transFlavors)) {
			QueryGenerator qg;
			boolean hasLocal = false;
			boolean hasSerial = false;
			for(DataFlavor tranFlav : transFlavors) {
				if(localQueryGeneratorFlavor.equals(tranFlav)) {
					hasLocal = true;
				} else if(serialQueryGeneratorFlavor.equals(tranFlav)) {
					hasSerial = true;
				}
			}
			try {
				if(hasLocal) {
					qg = (QueryGenerator)t.getTransferData(localQueryGeneratorFlavor);
				} else if(hasSerial) {
					qg = (QueryGenerator)t.getTransferData(serialQueryGeneratorFlavor);
				} else {
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
			qt.add(selPath, qg);
			return true;
		} else {
			return false;
		}
	}
	public boolean canImport(JComponent comp, DataFlavor[] transFlavors) {
		for(DataFlavor tranFlav : transFlavors) {
			if(localQueryGeneratorFlavor.equals(tranFlav) ||
					serialQueryGeneratorFlavor.equals(tranFlav)) {
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
			data = qt.getSelectionPath().getLastPathComponent();
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
					((Element)toSerialize).setAttribute("name", ((QueryGroup)data).getName());
					((QueryTreeModel)qt.getModel()).addQueryGroup(doc, toSerialize, ((QueryGroup)data));
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
