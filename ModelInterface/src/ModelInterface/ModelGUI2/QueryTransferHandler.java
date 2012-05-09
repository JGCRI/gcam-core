/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
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

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.DOMException;
import org.w3c.dom.ls.*;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.InputEvent;

import javax.swing.TransferHandler;
import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.tree.TreePath;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

import java.io.IOException;

import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.queries.SingleQueryExtension;
import ModelInterface.ModelGUI2.QueryTreeModel;
import ModelInterface.ModelGUI2.QueryTreeModel.QueryGroup;
import ModelInterface.common.DataPair;

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
	private TreePath[] dragPaths;

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
		dragPaths = null;
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
				qg = (QueryGenerator)t.getTransferData(localQueryGeneratorFlavor);
				qg.resetQueryBuilder();
			} else if(hasSerialGen) {
				qg = (QueryGenerator)t.getTransferData(serialQueryGeneratorFlavor);
				qg.resetQueryBuilder();
			} else if(hasLocalGroup && lastAction == MOVE) {
				qGroup = (QueryGroup)t.getTransferData(localQueryGroupFlavor);
				qGroup.resetQueryBuilders();
			} else if(hasSerialGroup) {
				qGroup = (QueryGroup)t.getTransferData(serialQueryGroupFlavor);
				qGroup.resetQueryBuilders();
			} else if(hasLocalNode) {
				// I don't think nodes are serializable..
				node = (Node)t.getTransferData(localNodeFlavor);
			} else if(hasSerialNode) {
				node = (Node)t.getTransferData(serialNodeFlavor);
			} else if(hasString) {
				String str = (String)t.getTransferData(DataFlavor.stringFlavor);
				LSInput tempInput = implls.createLSInput();
				tempInput.setStringData(str);
				node = doc.adoptNode(parser.parse(tempInput).getDocumentElement());
			} else {
				return false;
			}
		} catch(UnsupportedFlavorException ufe) {
			ufe.printStackTrace();
			// TODO: GUI exception
			// all though not sure if I will get this one
			return false;
		} catch(IOException ioe) {
			// TODO: GUI exception
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
	public void exportAsDrag(JComponent comp, InputEvent e, int action) {
		dragPaths = ((JTree)comp).getSelectionPaths();
		super.exportAsDrag(comp, e, action);
	}
	protected void exportDone(JComponent comp, Transferable t, int action) {
		lastAction = action;
		if(action == MOVE) {
			QueryTreeModel qt = (QueryTreeModel)((JTree)comp).getModel();
			TreePath[] paths;
			if(dragPaths != null) {
				paths = dragPaths;
				dragPaths = null;
			} else {
				paths = ((JTree)comp).getSelectionPaths();
			}
			for(TreePath path : paths) {
				if(!(path.getLastPathComponent() instanceof SingleQueryExtension.SingleQueryValue)) {
					qt.remove(path);
				}
			}
		} else if(action == NONE) {
			dragPaths = null;
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
			List<String> tempSingleNameList;
			if(paths.length == 1) {
				if(paths[0].getLastPathComponent() instanceof SingleQueryExtension.SingleQueryValue) {
					tempSingleNameList = ((SingleQueryExtension.SingleQueryValue)paths[0].getLastPathComponent()).getValues();
					Map.Entry<QueryGenerator, List<String>> tempPair =
						new DataPair<QueryGenerator, List<String>>(
								getSingleQueryParent(paths[0]), tempSingleNameList);
					data = convertSingleToQueryGenerator(tempPair);
				} else {
					data = paths[0].getLastPathComponent();
				}
			} else {
				ArrayList dataGrouped = new ArrayList(paths.length);
				Map<QueryGenerator, List<String>> tempSingleQueryMerge = 
					new HashMap<QueryGenerator, List<String>>();
				for(TreePath path : paths) {
					if(path.getLastPathComponent() instanceof SingleQueryExtension.SingleQueryValue) {
						QueryGenerator tempParent = getSingleQueryParent(path);
						tempSingleNameList = tempSingleQueryMerge.get(tempParent);
						if(tempSingleNameList == null) {
							tempSingleNameList = new ArrayList<String>();
						}
						tempSingleNameList.addAll(((SingleQueryExtension.SingleQueryValue)path.getLastPathComponent()).getValues());
						tempSingleQueryMerge.put(tempParent, tempSingleNameList);
					} else {
						dataGrouped.add(path.getLastPathComponent());
					}
				}
				// add all the merged single queries if any
				for(Iterator<Map.Entry<QueryGenerator, List<String>>> it = tempSingleQueryMerge.entrySet().iterator();
						it.hasNext(); ) {
					dataGrouped.add(convertSingleToQueryGenerator(it.next()));
				}
				data = ((QueryTreeModel)qt.getModel()).createQueryGroup("MultipleQuerySelection", dataGrouped);
			}
			transFlavors = new DataFlavor[4];
			if(data instanceof QueryGenerator) {
				transFlavors[0] = localQueryGeneratorFlavor;
				transFlavors[1] = serialQueryGeneratorFlavor;
			} else {
				transFlavors[0] = localQueryGroupFlavor;
				transFlavors[1] = serialQueryGroupFlavor;
			}
			transFlavors[2] = DataFlavor.stringFlavor;
			transFlavors[3] = localNodeFlavor;
			//transFlavors[4] = serialNodeFlavor;
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
					if(toSerialize.getFirstChild() == null) {
						// hack because the serializer doesn't seem to like no children on the
						// root tag
						toSerialize.appendChild(doc.createTextNode(""));
					}
				}
				if(flavor.isFlavorTextType()) {
					return implls.createLSSerializer().writeToString(toSerialize);
				} else {
					return toSerialize;
				}
			}
		}
		private QueryGenerator getSingleQueryParent(TreePath path) {
			if(!(path.getLastPathComponent() instanceof SingleQueryExtension.SingleQueryValue)) {
				// error?
				return null;
			}
			return (QueryGenerator)path.getParentPath().getLastPathComponent();
		}
		private QueryGenerator convertSingleToQueryGenerator(Map.Entry<QueryGenerator, List<String>> single) {
			QueryGenerator qgTemp = new QueryGenerator(single.getKey().getAsNode(doc));
			List<String> tempList = single.getValue();
			String singleTitle = tempList.size() == 1 ? tempList.get(0) :
				"single values ("+tempList.size()+")";
			qgTemp.setTitle(qgTemp.toString()+": "+singleTitle);
			qgTemp.setXPath(qgTemp.getXPath()+qgTemp
					.getForNodeLevelPath(tempList));
			qgTemp.setGroup(false);
			qgTemp.setBuildList(false);
			return qgTemp;
		}
	}
}
