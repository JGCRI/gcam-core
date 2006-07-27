package ModelInterface.ModelGUI2.queries;

import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.XMLDB;

import javax.swing.JList;
import javax.swing.JTree;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.ListSelectionModel;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.tree.TreePath;

import java.util.Map;
import java.util.Iterator;
import java.util.Vector;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.TreeMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlException;

public class LandAllocatorQueryBuilder extends QueryBuilder {
	public static Map varList;
	public static String xmlName = "LandAllocatorQuery";
	private JComponentAdapter listComp;
	private TreePath selectedTreePath;
	public LandAllocatorQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		listComp = null;
	}
	public String createListPath(int level) {
		System.out.println("This Method doesn't do anything");
		return null;
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		updateSelected(list);
		return updateList(list, label);
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		// better way than instanceof here?
		if(list instanceof JListAdapter) {
			return (new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					int[] selectedInd = list.getSelectedRows();
					if(selectedInd.length == 0 && qg.currSel != 0) {
						nextButton.setEnabled(false);
						cancelButton.setText(" Cancel "/*cancelTitle*/);
					} else {
						//cancelButton.setText("Finished");
						nextButton.setEnabled(true);
					}
				}
			});
		} else if(list instanceof JTreeAdapter) {
			return (new TreeSelectionListener() {
				public void valueChanged(TreeSelectionEvent e) {
					cancelButton.setText(list.getSelectedRows().length != 0 ? "Finished" : " Cancel ");
				}
			});
		} else {
			// error?
			return null;
		}
	}
	public void doFinish(JComponentAdapter list) {
		++qg.currSel;
		updateSelected(list);
		--qg.currSel;
		createXPath();
		//qg.levelValues = list.getSelectedValues();
		qg.levelValues = null;
		queryFunctions = null;
		queryFilter = null;
	}
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		return updateList(list, label);
	}
	public boolean isAtEnd() {
		return qg.currSel == 4-1;
	}
	/*
	private void getLeaves() {
		queryFilter = "";
		queryFunctions.clear();
		queryFunctions.add(DbViewer.xmlDB.getQueryFunctionAsDistinctNames());
		XmlResults res = DbViewer.xmlDB.createQuery("//LandLeaf/*[fn:count(child::text()) = 1]", queryFilter, queryFunctions);
		XmlValue val;
		try {
			while((val = res.next()) != null) {
				varList.put(val.asString(), false);
				val.delete();
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		DbViewer.xmlDB.printLockStats("LandAllocatorQueryBuilder.getLeaves");
	}
	*/
	private JTreeAdapter getLandUseTree() {
		// region query portion!!
		queryFilter = "/scenario/world/region";
		queryFunctions.clear();
		XmlResults res = DbViewer.xmlDB.createQuery("/LandAllocatorNode[@name='root']", queryFilter, queryFunctions);
		XmlValue val;
		// for some reason JTree doesn't accept Maps, only Hashtables
		Hashtable<String, Hashtable> landUseTree = new Hashtable<String, Hashtable>();
		try {
			while((val = res.next()) != null) {
				addToLandUseTree(val, landUseTree);
				val.delete();
			}
		} catch(XmlException xe) {
			xe.printStackTrace();
		}
		DbViewer.xmlDB.printLockStats("LandAllocatorQueryBuilder.getLandUseTree");
		JTree retTree = new JTree((Hashtable<String, Hashtable>)landUseTree);
		retTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		retTree.setSelectionRow(0);
		for(int i = 0; i < retTree.getRowCount(); ++i) {
			retTree.expandRow(i);
		}
		return new JTreeAdapter(retTree);
	}
	private void addToLandUseTree(XmlValue curr, Hashtable<String, Hashtable> tree) throws XmlException{
		Hashtable<String, Hashtable> currTree;
		String attr = XMLDB.getAttr(curr, "name");
		if(attr == null) {
			return;

		}
		String name = curr.getNodeName()+" "+attr;
		currTree = tree.get(name);
		if(currTree == null) {
			currTree = new Hashtable<String, Hashtable>();
			tree.put(name, currTree);
		}
		XmlValue val = curr.getFirstChild();
		XmlValue valPrev;
		while(val != null && !val.isNull()) {
			if(val.getType() == XmlValue.NODE && val.getNodeType() == XmlValue.ELEMENT_NODE) {
				addToLandUseTree(val, currTree);
			}
			valPrev = val;
			val = val.getNextSibling();
			valPrev.delete();
		}
	}

	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		Map temp = null;
		switch(qg.currSel) {
			case 2: {
					if(listComp != null) {
						list = listComp;
						listComp = null;
					}
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					//getLeaves();
					temp = varList;
					label.setText("Select Land Use Data:");
					Vector tempVector = new Vector();
					String[] currKeys = (String[])temp.keySet().toArray(new String[0]);
					((JList)list.getModel()).setListData(currKeys);
					// check the maps to see which ones are true and add it to the list of selected
					for (int i = 0; i < currKeys.length; ++i) {
						if (((Boolean)temp.get(currKeys[i])).booleanValue()) {
							tempVector.addElement(new Integer(i));
						}
					}
					int[] selected = new int[tempVector.size()];
					for (int i = 0; i < selected.length; i++) {
						selected[i] = ((Integer)tempVector.get(i)).intValue();
					}
					temp = null;
					tempVector = null;
					list.setSelectedRows(selected);
					return list;
			}
			case 3: {
					listComp = list;
					label.setText("Select Node to sum to:");
					return getLandUseTree();
			}
			default: System.out.println("Error currSel: "+qg.currSel);
				 return null;
		}
	}
	public void updateSelected(JComponentAdapter list) {
		Object[] selectedKeys = list.getSelectedValues();
		Map selected = null;
		switch(qg.currSel -1) {
			case 1: {
					break;
			}
			case 2: {
					selected = varList;
					for(Iterator it = selected.entrySet().iterator(); it.hasNext(); ) {
						((Map.Entry)it.next()).setValue(new Boolean(false));
					}
					for(int i = 0; i < selectedKeys.length; ++i) {
						selected.put(selectedKeys[i], new Boolean(true));
					}
					break;
			}
			case 3: {
					selectedTreePath = ((JTree)list.getModel()).getSelectionPath();
					break;
			}
			default: System.out.println("Error currSel: "+qg.currSel);
		}
	}
	private void createXPath() {
		String nameSel = null;
		for(Iterator i = varList.entrySet().iterator(); i.hasNext(); ) {
			Map.Entry me = (Map.Entry)i.next();
			if(((Boolean)me.getValue()).booleanValue()) {
				nameSel = (String)me.getKey();
			}
		}
		StringBuilder ret = new StringBuilder();
		Object[] selPath = selectedTreePath.getPath();
		String[] splitNodeName;
		for(int i = 1; i < selPath.length; ++i) {
			splitNodeName = selPath[i].toString().split(" ", 2);
			ret.append("/").append(splitNodeName[0]).append("[@name='").append(splitNodeName[1])
				.append("']");
		}
		ret.append("//").append(nameSel).append("/text()");
		qg.xPath = ret.toString();
		qg.axis1Name = qg.yearLevel = nameSel;
		qg.nodeLevel = selPath[selPath.length -1].toString();
		qg.var = qg.axis2Name = "Year";
		qg.group = false;
		qg.sumAll = false;
	}
	protected boolean isGlobal;
	public String getCompleteXPath(Object[] regions) {
		StringBuilder ret = new StringBuilder();
		boolean added = false;

		if(((String)regions[0]).equals("Global")) {
			ret.append(regionQueryPortion+"/");
			regions = new Object[0];
			isGlobal = true;
		} else {
			isGlobal = false;
		}
		for(int i = 0; i < regions.length; ++i) {
			if(!added) {
				ret.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1)).append(" and (");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(regions[i]).append("')");
		}
		if(added) {
			ret.append(" )]/");
		}
		return ret.append(qg.getXPath()).toString();
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Object[] ret = new Object[2];
		ret[0] = XMLDB.getAttr(n, "year");
		ret[1] = qg.nodeLevel.split(" ", 2)[1];
		DbViewer.xmlDB.printLockStats("LandAllocatorQueryBuilder.extractAxisInfo");
		return ret;
	}
	private boolean passedIt;
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			passedIt = false;
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		// is the nodeLevel always going to be the same as year if not need to add the check here
		String[] nodeLevelSplit = qg.nodeLevel.split(" ", 2);
		if(!passedIt && (nodeLevelSplit[0].equals(currNode.getNodeName()) && nodeLevelSplit[1].equals(XMLDB.getAttr(currNode, "name"))) ) {
			passedIt = true;
		}
		if(!passedIt && XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(qg.yearLevel) /*&& 
				!(nodeLevelSplit.equals(currNode.getNodeName()) && nodeLevelSplit.equals(XMLDB.getAttr(currNode, "name")))*/ ) {
			String attr = XMLDB.getAllAttr(currNode);
			attr = currNode.getNodeName()+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
			}
			currNode.delete();
			return (Map)tempMap.get(attr);
		} 
		currNode.delete();
		return tempMap;
	}
	public String getXMLName() {
		return xmlName;
	}
}
