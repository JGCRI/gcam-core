//package ModelGUI2;
package ModelInterface.ModelGUI2;

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

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class DemographicsQueryBuilder implements QueryBuilder {
	public static Map varList;
	protected Map popList;
	protected Map cohortList;
	protected Map genderList;
	protected QueryGenerator qg;
	public static String xmlName = "demographicsQuery";
	public DemographicsQueryBuilder(QueryGenerator qgIn) {
		qg = qgIn;
		popList = new LinkedHashMap();
		/*
		popList.put("populationMiniCAM", new Boolean(false));
		popList.put("populationSGMFixed", new Boolean(false));
		popList.put("populationSGMRate", new Boolean(false));
		*/
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
	public ListSelectionListener getListSelectionListener(final JList list, final JButton nextButton, final JButton cancelButton) {
		FileChooserDemo.xmlDB.setQueryFilter("/scenario/world/region/demographics/");
		FileChooserDemo.xmlDB.setQueryFunction("distinct-values(");
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedIndices();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
				} else if(qg.currSel < 5) {
					if(((String)list.getSelectedValues()[0]).equals("populationMiniCAM") && qg.currSel == 3) {
						nextButton.setEnabled(false);
						cancelButton.setText("Finished");
					} else {
						nextButton.setEnabled(true);
					}
				} else if(qg.currSel == 5) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				}
					/*
				 else if((qg.isSumable && (selectedInd[0] == 0 || selectedInd[0] == 1)) || selectedInd.length > 1
					|| ((String)list.getSelectedValues()[0]).startsWith("Group:")) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 3){
					nextButton.setEnabled(true);
					cancelButton.setText(" Cancel "/*cancelTitle/);
				}
			*/
			}
		});
	}
	public void doNext(JList list, JLabel label) {
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
		updateList(list, label);
	}
	public void doBack(JList list, JLabel label) {
		if(qg.currSel == 3) {
			cohortList = null;
		}
		updateList(list, label);
	}
	public void doFinish(JList list) {
		++qg.currSel;
		updateSelected(list);
		--qg.currSel;
		createXPath();
		if(isPopMiniCAMSelected()) {
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

		FileChooserDemo.xmlDB.setQueryFilter("");
		FileChooserDemo.xmlDB.setQueryFunction("");
	}
	public boolean isAtEnd() {
		return (qg.currSel == 3 && isPopMiniCAMSelected()) || qg.currSel == 5;
	}
	public void updateList(JList list, JLabel label) {
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
						FileChooserDemo.xmlDB.setQueryFunctionAsDistinctNames();
						popList = createList("*", false);
						FileChooserDemo.xmlDB.setQueryFunction("fn:distinct-values(");
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
		list.setListData(currKeys);
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
		list.setSelectedIndices(selected);
	}
	public void updateSelected(JList list) {
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
		XmlResults res = FileChooserDemo.xmlDB.createQuery("*/ageCohort[child::group[@name='"+gName+"']]/@ageGroup");
		try {
			while(res.hasNext()) {
				ret.append("(child::text()='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		FileChooserDemo.xmlDB.printLockStats("expandGroupName");
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
		// default axis1Name to nodeLevel
		qg.axis1Name = qg.nodeLevel;
		qg.axis2Name = "Year";
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
		if(isPopMiniCAMSelected()) {
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
		XmlResults res = FileChooserDemo.xmlDB.createQuery(path);
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
		FileChooserDemo.xmlDB.printLockStats("createList");
		return ret;
	}
	protected boolean isGlobal;
	public String getCompleteXPath(Object[] regions)  {
		boolean added = false;
		StringBuffer ret = new StringBuffer();
		if(((String)regions[0]).equals("Global")) {
			ret.append("region/");
			//regionSel = new int[0]; 
			regions = new Object[0];
			isGlobal = true;
		} else {
			isGlobal = false;
		}
		for(int i = 0; i < regions.length; ++i) {
			if(!added) {
				ret.append("region[ ");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(regions[i]).append("')");
		}
		if(added) {
			ret.append(" ]/");
		}
		return ret.append(qg.getXPath()).toString();
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Vector ret = new Vector(2,0);
		XmlValue nBefore;
		do {
			if(n.getNodeName().equals(qg.nodeLevel)) {
				ret.add(XMLDB.getAttr(n));
			} 
			if(n.getNodeName().equals(qg.yearLevel)) {
				ret.add(0, XMLDB.getAttr(n, "year"));
			} else if(XMLDB.hasAttr(n)) {
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
		FileChooserDemo.xmlDB.printLockStats("getRegionAndYearFromNode");
		return ret.toArray();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		// used to combine sectors and subsectors when possible to avoid large amounts of sparse tables
		/*
		if( (isGlobal && currNode.getNodeName().equals("region")) 
				|| (qg.nodeLevel.equals("supplysector") && currNode.getNodeName().equals("subsector")) 
				|| (qg.nodeLevel.matches(".*sector") && currNode.getNodeName().equals("technology"))) {
			currNode.delete();
			return tempMap;
		}
		*/
		if(currNode.getNodeName().equals("male") || currNode.getNodeName().equals("female")) {
			String attr = currNode.getNodeName();
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
			}
			currNode.delete();
			return (Map)tempMap.get(attr);
		} else if(XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(qg.nodeLevel) 
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
