package ModelInterface.ModelGUI2.queries;

import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.XMLDB;
import ModelInterface.common.DataPair;

import javax.swing.JList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.ListSelectionModel;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class SupplyDemandQueryBuilder extends QueryBuilder {
	public static Map varList;
	protected Map sectorList;
	protected Map subsectorList;
	protected Map techList;
	public static String xmlName = "supplyDemandQuery";
	public SupplyDemandQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		sectorList = null;
		subsectorList = null;
		techList = null;
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		queryFunctions.removeAllElements();
		queryFunctions.add("distinct-values");
		queryFilter = "/scenario/world/"+regionQueryPortion+"/";
		//DbViewer.xmlDB.setQueryFunction("distinct-values(");
		//DbViewer.xmlDB.setQueryFilter("/scenario/world/region/");
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedRows();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
				} else if(qg.currSel == 1 || qg.currSel == 2) {
					nextButton.setEnabled(true);
				} else if((qg.isSumable && (selectedInd[0] == 0 || selectedInd[0] == 1)) || selectedInd.length > 1
					|| ((String)list.getSelectedValues()[0]).startsWith("Group:")) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 5 && !qg.isSumable) {
					nextButton.setEnabled(true);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 5){
					nextButton.setEnabled(true);
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
		qg.levelValues = list.getSelectedValues();
		queryFunctions = null;
		queryFilter = null;
		//DbViewer.xmlDB.setQueryFunction("");
		//DbViewer.xmlDB.setQueryFilter("");
	}
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		// doing this stuff after currSel has changed now..
		// have to sub 1
		if(qg.currSel == 2) {
			sectorList = null;
		} else if(qg.currSel == 3) {
			subsectorList = null;
		} else if(qg.currSel == 4) {
			techList = null;
		}
		return updateList(list, label);
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		// being moved to after currSel changed, adjust numbers
		updateSelected(list);
		if(qg.currSel == 3) {
			for(Iterator it = varList.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry me = (Map.Entry)it.next();
				if(((Boolean)me.getValue()).booleanValue()) {
					qg.var = (String)me.getKey();
					qg.isSumable = qg.sumableList.contains(qg.var);
				}
			}
		}
		return updateList(list, label);
	}
	public boolean isAtEnd() {
		return qg.currSel == 6-1;
	}
	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		Map temp = null;
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					temp = varList;
					//list.setListData(varList.keySet().toArray());
					label.setText("Select Variable:");
					break;
			}
			case 3: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(sectorList == null) {
						sectorList = createList(sectorQueryPortion+"/@name", false);
						sectorList.putAll(createList(sectorQueryPortion+"/group/@name", true));
					}
					temp = sectorList;
					label.setText("Select Sector:");
					break;
			}
			case 4: {
					if(subsectorList == null) {
						subsectorList = createList(createListPath(4), false);
					}
					temp = subsectorList;
					label.setText("Select Subsector:");
					break;
			}
			case 5: {
					if(techList == null) {
						techList = createList(createListPath(5), false);
					}
					temp = techList;
					label.setText("Select Technology:");
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
		return list;
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
			case 3: {
					selected = sectorList;
					break;
			}
			case 4: {
					selected = subsectorList;
					break;
			}
			case 5: {
					selected = techList;
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
	public String createListPath(int level) {
		Map tempMap;
		StringBuffer ret = new StringBuffer();
		boolean added = false;
		boolean gq = false;
		if(level == -1) {
			gq = true;
			qg.sumAll = qg.group = false;
			level = 6;
		}
		for(int i = 0; i < level-3; ++i) {
			if(i == 0) {
				tempMap = sectorList;
				ret.append(sectorQueryPortion.substring(0, sectorQueryPortion.length()-1));
			} else if(i == 1){
				tempMap = subsectorList;
				ret.append(subsectorQueryPortion.substring(0, subsectorQueryPortion.length()-1));
			} else {
				tempMap = techList;
				ret.append(technologyQueryPortion.substring(0, technologyQueryPortion.length()-1));
				++i;
			}
			added = false;
			if(tempMap == null) {
				ret.append("]/");
				continue;
			}
			if(qg.isSumable && ((Boolean)tempMap.get("Sum All")).booleanValue()) {
				qg.sumAll = true;
			}
			if(qg.isSumable && ((Boolean)tempMap.get("Group All")).booleanValue()) {
				qg.group = true;
			}
			//for(Iterator it = tempMap.entrySet().iterator(); it.hasNext() && !((Boolean)tempMap.get("Sum All")).booleanValue(); ) 
			for(Iterator it = tempMap.entrySet().iterator(); it.hasNext() && !(qg.sumAll || qg.group); ) {
				Map.Entry me = (Map.Entry)it.next();
				if(((Boolean)me.getValue()).booleanValue()) {
					if(!added) {
						ret.append(" and (");
						added = true;
					} else {
						ret.append(" or ");
					}
					if(!gq && ((String)me.getKey()).startsWith("Group:")) {
						ret.append(expandGroupName(((String)me.getKey()).substring(7)));
						gq = true;
					} else {
						ret.append("(@name='"+me.getKey()+"')");
					}
				}
			}
			if(added) {
				ret.append(" )]/");
			} else {
				ret.append("]/");
			}
		}
		if(level == 3) {
			ret.append(sectorQueryPortion+"/@name");
		} else if(level == 4) {
			ret.append(subsectorQueryPortion+"/@name");
		} else if(level == 5) {
			ret.append(technologyQueryPortion+"/@name");
		} else {
			ret.append(qg.var).append("/node()");
			//ret += "period/"+var+"/node()";
			System.out.println("The xpath is: "+ret.toString());
		}
		if(gq) {
			qg.group = true;
			qg.sumAll = true;
		}
		return ret.toString();
	}
	private String expandGroupName(String gName) {
		String query;
		StringBuffer ret = new StringBuffer();
		if(qg.currSel == 3) {
			//query = "supplysector";
			query = sectorQueryPortion;
		} else if(qg.currSel == 4) {
			query = sectorQueryPortion+"/"+subsectorQueryPortion;
		} else {
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion;
		}
		XmlResults res = DbViewer.xmlDB.createQuery(queryFilter+query+"[child::group[@name='"+gName+"']]/@name", 
				queryFunctions, null, null);
		try {
			while(res.hasNext()) {
				ret.append("(@name='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		DbViewer.xmlDB.printLockStats("expandGroupName");
		return ret.toString();
	}
	private void createXPath() {
		String yearLevel = null;
		if(qg.isSumable) {
			qg.xPath = createListPath(6);
			if(QueryGenerator.hasYearList.contains(qg.var)) {
				yearLevel = qg.var;
			} else {
				yearLevel = "technology";
			}
		} else {
			qg.xPath = createListPath(qg.currSel+1);
			qg.xPath = qg.xPath.replaceFirst("/[^/]*/[^/]*$", "/" + qg.var + "/text()");
			if(qg.currSel == 5) {
				if(QueryGenerator.hasYearList.contains(qg.var)) {
					yearLevel = qg.var;
				} else {
					yearLevel = "technology";
				}
			} else {
				yearLevel = qg.var;
			}
		}
		qg.yearLevel = new DataPair<String, String>(yearLevel, "year");
		String nodeLevel = null;
		switch(qg.currSel) {
			case 3: nodeLevel = "sector";
				break;
			case 4: nodeLevel = "subsector";
				break;
			case 5: nodeLevel = "technology";
				break;
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		// what about input?
		qg.nodeLevel = new DataPair<String, String>(nodeLevel, "name");
		// default axis1Name to nodeLevel
		qg.axis1Name = nodeLevel;
		qg.axis2Name = "Year";
	}
	private Map createList(String path, boolean isGroupNames) {
		LinkedHashMap ret = new LinkedHashMap();
		if(!isGroupNames && qg.isSumable) {
			ret.put("Sum All", new Boolean(false));
			ret.put("Group All", new Boolean(false));
		}
		XmlResults res = DbViewer.xmlDB.createQuery(queryFilter+path, queryFunctions, null, null);
		try {
			while(res.hasNext()) {
				if(!isGroupNames) {
					ret.put(res.next().asString(), new Boolean(false));
				} else { 
					ret.put("Group: "+res.next().asString(), new Boolean(false));
				}
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		res.delete();
		DbViewer.xmlDB.printLockStats("createList");
		return ret;
	}
	public String getCompleteXPath(Object[] regions) {
		boolean added = false;
		StringBuffer ret = new StringBuffer();
		if(((String)regions[0]).equals("Global")) {
			ret.append(regionQueryPortion+"/");
			//regionSel = new int[0]; 
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
		Vector ret = new Vector(2, 0);
		XmlValue nBefore;
		XmlValue delTemp;
		if(qg.nodeLevel.getKey().equals("keyword")) {
			nBefore = n.getNextSibling();
			while(!nBefore.isNull()) {
				if(nBefore.getNodeName().equals(qg.nodeLevel.getKey()) &&
					XMLDB.getAttr(nBefore, qg.nodeLevel.getValue()) != null) {
					ret.add(XMLDB.getAttr(nBefore, qg.nodeLevel.getValue()));
					break;
				}
				delTemp = nBefore;
				nBefore = nBefore.getNextSibling();
				delTemp.delete();
			}
			nBefore.delete();
		}
		do {
			if(qg.nodeLevel.getKey().equals(XMLDB.getAttr(n, "type")) || qg.nodeLevel.getKey().equals(n.getNodeName())) {
				if(qg.nodeLevel.getValue() == null) {
					String nameAttrVal = XMLDB.getAttr(n, "name");
					if(nameAttrVal == null) {
						nameAttrVal = XMLDB.getAttr(n, "fuel-name");
					}
					// else problems!!
					ret.add(nameAttrVal);
				} else {
					ret.add(XMLDB.getAttr(n, qg.nodeLevel.getValue()));
				}
			} 
			if(qg.yearLevel.getKey().equals(XMLDB.getAttr(n, "type")) || qg.yearLevel.getKey().equals(n.getNodeName())) {
				if(qg.yearLevel.getValue() == null) {
					ret.add(0, XMLDB.getAttr(n, "year"));
				} else {
					ret.add(0, XMLDB.getAttr(n, qg.yearLevel.getValue()));
				}
			}/* else if(XMLDB.hasAttr(n)) {
				// are filter maps used, I don't belive filtering is currently enabled for DB Output
				// is this a feature people would want?
				Map tempFilter;
				if (filterMaps.containsKey(n.getNodeName())) {
					tempFilter = (Map)filterMaps.get(n.getNodeName());
				} else {
					tempFilter = new HashMap();
				}
				String attr = XMLDB.getAttr(n);
				if (!tempFilter.containsKey(attr)) {
					tempFilter.put(attr, new Boolean(true));
					filterMaps.put(n.getNodeName(), tempFilter);
				}
			}
			*/
			nBefore = n;
			n = n.getParentNode();
			nBefore.delete();
		} while(n.getNodeType() != XmlValue.DOCUMENT_NODE); 
		n.delete();
		DbViewer.xmlDB.printLockStats("SupplyDemandQueryBuilder.getRegionAndYearFromNode");
		return ret.toArray();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		String type = XMLDB.getAttr(currNode, "type");
		if(type == null) {
			type = currNode.getNodeName();
		}
		// used to combine sectors and subsectors when possible to avoid large amounts of sparse tables
		if( (isGlobal && type.equals("region")) || qg.getCollapseOnList().contains(type)) {
			currNode.delete();
			return tempMap;
		}
		if(XMLDB.hasAttr(currNode) && !type.equals(qg.nodeLevel.getKey()) 
				&& !type.equals(qg.yearLevel.getKey())) {
			String attr = XMLDB.getAllAttr(currNode);
			//attr = currNode.getNodeName()+"@"+attr;
			if(qg.labelRewriteMap != null && qg.labelRewriteMap.containsKey(type)) {
				Map<String, String> currRewriteMap = qg.labelRewriteMap.get(type);
				if(currRewriteMap.containsKey(attr)) {
					attr = currRewriteMap.get(attr);
				}
			}
			attr = type+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new HashMap());
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
	public List<String> getDefaultCollpaseList() {
		List<String> defaultCollapse = new Vector<String>(3);
		defaultCollapse.add("sector");
		defaultCollapse.add("subsector");
		defaultCollapse.add("technology");
		return defaultCollapse;
	}
}
