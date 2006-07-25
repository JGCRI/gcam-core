package ModelInterface.ModelGUI2.queries;

import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.XMLDB;

import javax.swing.JList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.ListSelectionModel;

import java.util.Map;
import java.util.Iterator;
import java.util.Vector;
import java.util.HashMap;
import java.util.TreeMap;

import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlException;

public class LandAllocatorQueryBuilder extends QueryBuilder {
	public static Map varList;
	public static String xmlName = "LandAllocatorQuery";
	public LandAllocatorQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		varList = new HashMap();
	}
	public String createListPath(int level) {
		System.out.println("This Method doesn't do anything");
		return null;
	}
	public void doNext(JComponentAdapter list, JLabel label) {
		updateList(list, label);
	}
	public ListSelectionListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedRows();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
				} else {
					cancelButton.setText("Finished");
				}
			}
		});
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
	public void doBack(JComponentAdapter list, JLabel label) {
		System.out.println("Would I do anything here");
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	private void getLeaves() {
		// region query portion!!
		queryFilter = "/scenario/world/region";
		queryFunctions.clear();
		//queryFunctions.add("distinct-values");
		XmlResults res = DbViewer.xmlDB.createQuery("/LandAllocatorNode[@name='root']", queryFilter, queryFunctions);
		XmlValue val;
		Map<String, Map> landUseTree = new HashMap<String, Map>();
		System.out.println("Did query");
		try {
			while((val = res.next()) != null) {
				System.out.println("Got Val: "+val.getNodeName());
				varList.put(val.getNodeName(), false);
				addToLandUseTree(val, landUseTree);
				val.delete();
			}
			System.out.println("Land Use Map: "+landUseTree);
		} catch(XmlException xe) {
			xe.printStackTrace();
		}
		DbViewer.xmlDB.printLockStats("LandAllocatorQueryBuilder.getLeaves");
	}
	private void addToLandUseTree(XmlValue curr, Map<String, Map> tree) throws XmlException{
		Map<String, Map> currTree;
		String attr = XMLDB.getAttr(curr, "name");
		if(attr == null) {
			return;

		}
		String name = curr.getNodeName()+" "+attr;
		System.out.println("Curr Name: "+name);
		currTree = tree.get(name);
		if(currTree == null) {
			currTree = new HashMap<String, Map>();
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

	public void updateList(JComponentAdapter list, JLabel label) {
		Map temp = null;
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					getLeaves();
					temp = varList;
					label.setText("Select Cost Type:");
					break;
			}
			default: System.out.println("Error currSel: "+qg.currSel);
		}
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
	}
	public void updateSelected(JComponentAdapter list) {
		Object[] selectedKeys = list.getSelectedValues();
		Map selected = null;
		switch(qg.currSel -1) {
			case 1: {
					return;
			}
			case 2: {
					selected = varList;
					break;
			}
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		for(Iterator it = selected.entrySet().iterator(); it.hasNext(); ) {
			((Map.Entry)it.next()).setValue(new Boolean(false));
		}
		for(int i = 0; i < selectedKeys.length; ++i) {
			selected.put(selectedKeys[i], new Boolean(true));
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
		qg.xPath = "/LandLeaf[@name='"+nameSel+"']/land-allocation/text()";
		qg.axis1Name = "Land Allocation";
		qg.yearLevel = qg.nodeLevel = "land-allocation";
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
		ret.append("LandAllocatorNode[@name='root']/");
		return ret.append(qg.getXPath()).toString();
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Object[] ret = new Object[2];
		ret[0] = XMLDB.getAttr(n, "year");
		ret[1] = "land-allocation";
		DbViewer.xmlDB.printLockStats("LandAllocatorQueryBuilder.extractAxisInfo");
		return ret;
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		// is the nodeLevel always going to be the same as year if not need to add the check here
		if(XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(qg.yearLevel)) {
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
