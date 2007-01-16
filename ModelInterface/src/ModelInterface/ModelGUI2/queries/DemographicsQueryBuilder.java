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

public class DemographicsQueryBuilder extends QueryBuilder {
	public static Map varList;
	protected Map popList;
	protected Map cohortList;
	protected Map genderList;
	public static String xmlName = "demographicsQuery";
	public DemographicsQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		popList = new LinkedHashMap();
		popList = null;
		cohortList = null;
		genderList = new LinkedHashMap();
		genderList.put("male", new Boolean(false));
		genderList.put("female", new Boolean(false));
	}
	private boolean isPopMiniCAMSelected() {
		if(popList == null) {
			return false;
		}
		Object got = popList.get("populationMiniCAM");
		return got != null && ((Boolean)got).booleanValue();
	}
	private boolean isTotalPopulation() {
		if(varList == null) {
			return false;
		}
		Object got = varList.get("total-population");
		return got != null && ((Boolean)got).booleanValue();
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		queryFunctions.removeAllElements();
		queryFunctions.add("distinct-values");
		queryFilter = "/scenario/world/"+regionQueryPortion+"/demographics/";
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedRows();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
				} else if(qg.currSel < 5) {
					if((((String)list.getSelectedValues()[0]).equals("populationMiniCAM") ||
							isTotalPopulation()) && qg.currSel == 3) {
						nextButton.setEnabled(false);
						cancelButton.setText("Finished");
					} else {
						nextButton.setEnabled(true);
					}
				} else if(qg.currSel == 5) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				}
			}
		});
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
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
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		if(qg.currSel == 3) {
			cohortList = null;
		}
		return updateList(list, label);
	}
	public void doFinish(JComponentAdapter list) {
		++qg.currSel;
		updateSelected(list);
		--qg.currSel;
		createXPath();
		if(isPopMiniCAMSelected() || isTotalPopulation()) {
			qg.levelValues = list.getSelectedValues(); // doesn't make sense?
		} else {
			Vector temp = new Vector();
			for(Iterator it = cohortList.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry me = (Map.Entry)it.next();
				if(((Boolean)me.getValue()).booleanValue()) {
					temp.add(me.getKey());
				}
			}
			qg.levelValues = temp.toArray();
		}

		queryFunctions = null;
		queryFilter = null;
		//DbViewer.xmlDB.setQueryFilter("");
		//DbViewer.xmlDB.setQueryFunction("");
	}
	public boolean isAtEnd() {
		// did not update this since it is going to be removed anyways
		return (qg.currSel == 3 && isPopMiniCAMSelected()) || qg.currSel == 5;
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
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					if(popList == null) {
						Vector funcTemp = queryFunctions;
						queryFunctions = new Vector<String>(1,0);
						queryFunctions.add(DbViewer.xmlDB.getQueryFunctionAsDistinctNames());
						popList = createList("*", false);
						queryFunctions = funcTemp;
					}
					temp = popList;
					label.setText("Select Population Type:");
					break;
			}
			case 4: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(cohortList == null) {
						cohortList = createList("*/ageCohort/@ageGroup", false);
						cohortList.putAll(createList("*/ageCohort/group/@name", true));
					}
					temp = cohortList;
					label.setText("Select Age Cohorts:");
					break;
			}
			case 5: {
					temp = genderList;
					label.setText("Select Genders: ");
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
		switch(qg.currSel) { 
			case 2: {
					return;
			}
			case 3: {
					selected = varList;
					break;
			}
			case 4: {
					selected = popList;
					break;
			}
			case 5: {
					selected = cohortList;
					break;
			}
			case 6: {
					selected = genderList;
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
	private String expandGroupName(String gName) {
		StringBuffer ret = new StringBuffer();
		XmlResults res = DbViewer.xmlDB.createQuery("*/ageCohort[child::group[@name='"+gName+"']]/@ageGroup",
				queryFilter, queryFunctions);
		try {
			while(res.hasNext()) {
				ret.append("(child::text()='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		DbViewer.xmlDB.printLockStats("expandGroupName");
		return ret.toString();
	}
	private void createXPath() {
		qg.xPath = createListPath(0);
		if(isPopMiniCAMSelected()) {
			qg.yearLevel = "populationMiniCAM";
			qg.nodeLevel = "region";
		} else if(popList.get("populationSGMFixed") != null && ((Boolean)popList.get("populationSGMFixed")).booleanValue()) {
			qg.yearLevel = "populationSGMFixed";
			qg.nodeLevel = "ageCohort";
		} else {
			qg.yearLevel = "populationSGMRate";
			qg.nodeLevel = "ageCohort";
		}
		if(isTotalPopulation()) {
			// overwrite the nodeLevel if totalPopulation, since it is
			// really the same fore MiniCAM or SGM
			qg.nodeLevel = "region";
		}
		// default axis1Name to nodeLevel
		qg.axis1Name = qg.nodeLevel;
		qg.axis2Name = "Year";
		// should total-population be group?
		qg.group = true;
	}
	public String createListPath(int level) {
		StringBuffer ret = new StringBuffer("demographics/");
		boolean added = false;
		for(Iterator it = popList.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry me = (Map.Entry)it.next();
			if(((Boolean)me.getValue()).booleanValue()) {
				ret.append(me.getKey()).append("/");
				break;
			}
		}
		if(isPopMiniCAMSelected() || isTotalPopulation()) {
			return ret.append(qg.var).append("/node()").toString();
		}
		ret.append("ageCohort");
		for(Iterator it = cohortList.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry me = (Map.Entry)it.next();
			if(((Boolean)me.getValue()).booleanValue()) {
				if(!added) {
					ret.append("[ ");
					added = true;
				} else {
					ret.append(" or ");
				}
				if(((String)me.getKey()).startsWith("Group:")) {
					ret.append(expandGroupName(((String)me.getKey()).substring(7)));
				} else {
					ret.append("(@ageGroup='"+me.getKey()+"')");
				}
			}
		}
		if(added) {
			ret.append(" ]/");
		} else {
			ret.append("/");
		}
		if(popList.get("populationSGMFixed") != null && ((Boolean)popList.get("populationSGMFixed")).booleanValue()) {
			boolean mSel = ((Boolean)genderList.get("male")).booleanValue();
			boolean fSel = ((Boolean)genderList.get("female")).booleanValue();
			if(mSel && fSel) {
				ret.append("*/");
			} else if(mSel) {
				ret.append("male/");
			} else {
				ret.append("female/");
			}
		} else {
			added = false;
			ret.append("gender");
			for(Iterator it = genderList.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry me = (Map.Entry)it.next();
				if(((Boolean)me.getValue()).booleanValue()) {
					if(!added) {
						ret.append("[ ");
						added = true;
					} else {
						ret.append(" or ");
					}
					ret.append("(@type = '"+me.getKey()+"')");
				}
			}
			if(added) {
				ret.append(" ]/");
			} else {
				ret.append("/");
			}
		}
		ret.append(qg.var).append("/node()");
		//ret += "period/"+var+"/node()";
		System.out.println("The xpath is: "+ret.toString());
		return ret.toString();
	}
	private Map createList(String path, boolean isGroupNames) { 
		System.out.println("Query path: "+path);
		LinkedHashMap ret = new LinkedHashMap();
		/*
		if(!isGroupNames && qg.isSumable) {
			ret.put("Sum All", new Boolean(false));
			ret.put("Group All", new Boolean(false));
		}
		*/
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
	public String getCompleteXPath(Object[] regions)  {
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
				ret.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1) + " and (");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(regions[i]).append("')");
		}
		if(!isGlobal) {
			ret.append(" )]/");
		}
		return ret.append(qg.getXPath()).toString();
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Vector ret = new Vector(2,0);
		XmlValue nBefore;
		do {
			if(qg.nodeLevel.equals(XMLDB.getAttr(n, "type"))) {
				ret.add(XMLDB.getAttr(n, "name"));
			} else if(n.getNodeName().equals(qg.nodeLevel)) {
				ret.add(XMLDB.getAttr(n, "ageGroup"));
			}
			if(n.getNodeName().equals(qg.yearLevel)) {
				ret.add(0, XMLDB.getAttr(n, "year"));
			} else if(XMLDB.hasAttr(n)) {
				// TODO: figure out if we really want to be able to filter
				HashMap tempFilter;
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
		DbViewer.xmlDB.printLockStats("getRegionAndYearFromNode");
		return ret.toArray();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		// used to combine paths when possible to avoid large amounts of sparse tables
		if(currNode.getNodeName().equals("male") || currNode.getNodeName().equals("female")) {
			String attr = currNode.getNodeName();
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
			}
			currNode.delete();
			return (Map)tempMap.get(attr);
		} else if(XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(qg.nodeLevel) 
				&& !qg.nodeLevel.equals(XMLDB.getAttr(currNode, "type"))
				&& !currNode.getNodeName().equals(qg.yearLevel)) {
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
