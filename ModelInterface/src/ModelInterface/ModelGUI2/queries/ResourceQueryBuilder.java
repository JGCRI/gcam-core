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
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class ResourceQueryBuilder extends QueryBuilder {
	static Map varList;
	protected Map resourceList;
	protected Map subresourceList;
	protected Map gradeList;
	public static String xmlName = "resourceQuery";
	public ResourceQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		varList = new LinkedHashMap();
		varList.put("available", new Boolean(false));
		varList.put("cost", new Boolean(false));
		resourceList = null;
		subresourceList = null;
		gradeList = null;
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		queryFunctions.removeAllElements();
		queryFunctions.add("distinct-values");
		queryFilter = "/scenario/world/"+regionQueryPortion+"/";
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
			resourceList = null;
		} else if(qg.currSel == 3) {
			subresourceList = null;
		} else if(qg.currSel == 4) {
			gradeList = null;
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
					//System.out.println("var is "+var);
					qg.isSumable = qg.sumableList.contains(qg.var);
					/*
					if(isSumable) {
						System.out.println("it does contain it");
					} else {
						System.out.println("doesn't contain it");
					}
					*/
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
					label.setText("Select Variable:");
					break;
			}
			case 3: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(resourceList == null) {
						resourceList = createList(resourceQueryPortion+"/@name", false);
						resourceList.putAll(createList(resourceQueryPortion+"/group/@name", true));
					}
					temp = resourceList;
					label.setText("Select Resource:");
					break;
			}
			case 4: {
					if(subresourceList == null) {
						subresourceList = createList(createListPath(4), false);
					}
					temp = subresourceList;
					label.setText("Select Sub Resource:");
					break;
			}
			case 5: {
					if(gradeList == null) {
						gradeList = createList(createListPath(5), false);
					}
					temp = gradeList;
					label.setText("Select Grade:");
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
					selected = resourceList;
					break;
			}
			case 4: {
					selected = subresourceList;
					break;
			}
			case 5: {
					selected = gradeList;
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
			added = false;
			if(i == 0) {
				tempMap = resourceList;
				ret.append(resourceQueryPortion.substring(0,resourceQueryPortion.length()-1));
			} else if(i == 1){
				tempMap = subresourceList;
				ret.append(subresourceQueryPortion.substring(0, subresourceQueryPortion.length()-1));
			} else {
				tempMap = gradeList;
				ret.append("grade");
				++i;
			}
			if(tempMap == null) {
				if(i <= 1) {
					ret.append("]/");
				} else {
					ret.append("/");
				}
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
						if(i <= 1) {
							ret.append(" and ( ");
						} else { 
							ret.append("[(");
						}
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
				System.out.println("Ends up here!!!");
				ret.append("/");
			}
		}
		if(level == 4) {
			ret.append(subresourceQueryPortion+"/@name");
		} else if(level == 5) {
			ret.append("grade/@name");
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
			query = resourceQueryPortion.substring(0, resourceQueryPortion.length()-1)+" and child::group[@name='"+gName+"']]/@name";
		} else if(qg.currSel == 4) {
			query = subresourceQueryPortion.substring(0, subresourceQueryPortion.length()-1)+" and child::group[@name='"+gName+"']]/@name";
		} else {
			query = "*/"+subresourceQueryPortion+"/grade[child::group[@name='"+gName+"']]/@name";
		}
		//XmlResults res = DbViewer.xmlDB.createQuery(query+"[child::group[@name='"+gName+"']]/@name");
		XmlResults res = DbViewer.xmlDB.createQuery(query, queryFilter, queryFunctions);
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
		qg.xPath = createListPath(6);
		switch(qg.currSel) {
			case 3: qg.nodeLevel = "resource";
				qg.axis1Name = "resource";
				break;
			case 4: qg.nodeLevel = "subresource";
				qg.axis1Name = qg.nodeLevel;
				break;
			case 5: qg.nodeLevel = "grade";
				qg.axis1Name = qg.nodeLevel;
				break;
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		// default axis1Name to nodeLevel
		qg.yearLevel = qg.var;
		qg.axis2Name = "Year";
	}
	private Map createList(String path, boolean isGroupNames) {
		LinkedHashMap ret = new LinkedHashMap();
		if(!isGroupNames && qg.isSumable) {
			ret.put("Sum All", new Boolean(false));
			ret.put("Group All", new Boolean(false));
		}
		XmlResults res = DbViewer.xmlDB.createQuery(path, queryFilter, queryFunctions);
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
	protected boolean isGlobal;
	public String getCompleteXPath(Object[] regions) {
		boolean added = false;
		StringBuffer ret = new StringBuffer();
		if(((String)regions[0]).equals("Global")) {
			ret.append(regionQueryPortion).append("/");
			//regionSel = new int[0]; 
			regions = new Object[0];
			isGlobal = true;
		} else {
			isGlobal = false;
		}
		for(int i = 0; i < regions.length; ++i) {
			if(!added) {
				ret.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1)+" and (");
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
		do {
			if(n.getNodeName().matches(qg.nodeLevel) || qg.nodeLevel.equals(XMLDB.getAttr(n, "type"))) {
				ret.add(XMLDB.getAttr(n, "name"));
			} 
			if(n.getNodeName().equals(qg.yearLevel)) {
				ret.add(0, XMLDB.getAttr(n, "year"));
				/*
				//ret.add(n.getAttributes().getNamedItem("name").getNodeValue());
				if(!getOneAttrVal(n).equals("fillout=1")) {
				ret.add(getOneAttrVal(n));
				} else {
				ret.add(getOneAttrVal(n, 1));
				}
				*/

			} else if(XMLDB.hasAttr(n)) {
				Map tempFilter;
				if (filterMaps.containsKey(n.getNodeName())) {
					tempFilter = (HashMap)filterMaps.get(n.getNodeName());
				} else {
					tempFilter = new HashMap();
				}
				String attr = XMLDB.getAttr(n);
				if (!tempFilter.containsKey(attr)) {
					tempFilter.put(attr, new Boolean(true));
					filterMaps.put(n.getNodeName(), tempFilter);
				}
			}
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
		if( (isGlobal && type.equals("region")) 
				|| (qg.nodeLevel.equals("resource") && type.equals("subresource")) 
				|| (qg.nodeLevel.matches(".*resource") && type.equals("grade"))) {
			currNode.delete();
			return tempMap;
		}
		if(XMLDB.hasAttr(currNode) && !type.equals(qg.nodeLevel) 
				&& !type.equals(qg.yearLevel)) {
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
