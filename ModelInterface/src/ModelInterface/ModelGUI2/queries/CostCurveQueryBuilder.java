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
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class CostCurveQueryBuilder extends QueryBuilder {
	public static Map varList;
	public static String xmlName = "costCurveQuery";
	public CostCurveQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		varList = new HashMap();
		varList.put("PeriodCostCurves", false);
		varList.put("RegionalCostCurvesByPeriod", false);
		varList.put("RegionalUndiscountedCosts", false);
		varList.put("RegionalDiscountedCosts", false);
	}
	public String createListPath(int level) {
		System.out.println("This Method doesn't do anything");
		return null;
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		//System.out.println("This Method doesn't do anything");
		return updateList(list, label);
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
					/*
				} else if(qg.currSel == 1 || qg.currSel == 2) {
					nextButton.setEnabled(true);
				} else if((qg.isSumable && (selectedInd[0] == 0 || selectedInd[0] == 1)) || selectedInd.length > 1
					|| ((String)list.getSelectedValues()[0]).startsWith("Group:")) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 5){
					nextButton.setEnabled(true);
					cancelButton.setText(" Cancel "/*cancelTitle/);
					*/
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
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		System.out.println("Would I do anything here");
		return list;
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		Map temp = null;
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
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
		String typeSel = null;
		for(Iterator i = varList.entrySet().iterator(); i.hasNext(); ) {
			Map.Entry me = (Map.Entry)i.next();
			if(((Boolean)me.getValue()).booleanValue()) {
				typeSel = (String)me.getKey();
			}
		}
		if(typeSel.equals("RegionalDiscountedCosts")) {
			qg.xPath = "/text()";
			qg.axis1Name = "DiscountedCost";
			qg.yearLevel = new DataPair<String, String>("DiscountedCost", null);
			qg.nodeLevel = new DataPair<String, String>("DiscountedCost", null);
			qg.axis2Name = "Region";
			qg.var = "Discounted Cost";
		} else if(typeSel.equals("RegionalUndiscountedCosts")) {
			qg.xPath = "/text()";
			qg.axis1Name = "UndiscountedCost";
			qg.yearLevel = new DataPair<String, String>("UndiscountedCost", null);
			qg.nodeLevel = new DataPair<String, String>("UndiscountedCost", null);
			qg.axis2Name = "Region";
			qg.var = "Undiscounted Cost";
		} else {
			qg.xPath = "PointSet/DataPoint/y/text()";
			qg.nodeLevel = new DataPair<String, String>("Curve", null);
			qg.yearLevel = new DataPair<String, String>("DataPoint", null);
			qg.axis1Name = "Region";
			qg.var = "Cost";
			if(typeSel.equals("RegionalCostCurvesByPeriod")) {
				qg.axis2Name = "Year";
			} else {
				qg.axis2Name = "Iteration";
			}
		}
		qg.group = false;
		qg.sumAll = false;
	}
	public String getCompleteXPath(Object[] regions) {
		System.out.println("Trying to complete xpath");
		StringBuffer strBuff = new StringBuffer();
		if(qg.nodeLevel.equals("DiscountedCost") || qg.nodeLevel.equals("UndiscountedCost")) {
			strBuff.append("CostCurvesInfo/").append("Regional").append(qg.nodeLevel).append("s/").append(qg.nodeLevel);
			if(((String)regions[0]).equals("Global")) {
				strBuff.append("/").append(qg.xPath);
				isGlobal = true;
				return strBuff.toString();
			} else {
				isGlobal = false;
				strBuff.append("[");
				for(int i = 0; i < regions.length; ++i) {
					strBuff.append(" (@name='").append(regions[i]).append("') or ");
				}
				strBuff.replace(strBuff.length()-3, strBuff.length(), "]/");
				strBuff.append(qg.xPath);
				return strBuff.toString();
			}
		} else if(qg.axis2Name.equals("Year")) {
			strBuff.append("CostCurvesInfo/RegionalCostCurvesByPeriod/Curve");
			if(((String)regions[0]).equals("Global")) {
				strBuff.append("/").append(qg.xPath);
				isGlobal = true;
				return strBuff.toString();
			} else {
				isGlobal = false;
				strBuff.append("[ child::title[");
				for(int i = 0; i < regions.length; ++i) {
					strBuff.append(" (child::text()='").append(regions[i]).append("') or ");
				}
				strBuff.replace(strBuff.length()-3, strBuff.length(), "] ]/");
				strBuff.append(qg.xPath);
				System.out.println("Returning xp: "+strBuff.toString());
				return strBuff.toString();
			}
		} else {
			strBuff.append("CostCurvesInfo/PeriodCostCurves/CostCurves/Curve");
			if(((String)regions[0]).equals("Global")) {
				strBuff.append("/").append(qg.xPath);
				isGlobal = true;
				return strBuff.toString();
			} else {
				isGlobal = false;
				strBuff.append("[ child::title[");
				for(int i = 0; i < regions.length; ++i) {
					strBuff.append(" (matches(child::text(),'").append(regions[i]).append("')) or ");
				}
				strBuff.replace(strBuff.length()-3, strBuff.length(), "] ]/");
				strBuff.append(qg.xPath);
				System.out.println("Returning xp: "+strBuff.toString());
				return strBuff.toString();
			}
		}
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Vector ret = new Vector(2, 0);
		XmlValue nBefore;
		do {
			if(n.getNodeName().equals(qg.nodeLevel.getKey())) {
				if(qg.nodeLevel.getKey().equals("UndiscountedCost") || qg.nodeLevel.getKey().equals("DiscountedCost")) {
					ret.add(qg.nodeLevel.getKey());
					/*
				} else if(qg.yearLevel.equals("DataPoint")) {
					// check the locks after this line, It might leave some
					ret.add(n.getFirstChild().getFirstChild().getNodeValue());
					*/
				} else if(isGlobal) {
					ret.add("Global");
				} else {
					// check the locks after this line, It might leave some
					//ret.add(n.getFirstChild().getFirstChild().getNodeValue());
					/*
					System.out.println("Nodel level node name: "+n.getNodeName());
					System.out.println("FC Node Name: "+n.getFirstChild().getNodeName());
					System.out.println("FC Node Value: "+n.getFirstChild().getNodeValue());
					System.out.println("FC Node Type: "+n.getFirstChild().getNodeType());
					XmlValue tn = n.getFirstChild();
					while((tn = tn.getNextSibling()).getNodeType() != 3) {
						System.out.println("FC Node Name: "+tn.getNodeName());
						System.out.println("FC Node Value: "+tn.getNodeValue());
						System.out.println("FC Node Type: "+tn.getNodeType());
					}
					System.out.println("after FC Node Name: "+tn.getNodeName());
					System.out.println("after FC Node Value: "+tn.getNodeValue());
					System.out.println("after FC Node Type: "+tn.getNodeType());

					ret.add(tn.getFirstChild().getFirstChild().getNodeValue());
					*/
					ret.add(n.getFirstChild().getNextSibling().getFirstChild().getNodeValue());
				}
					
				//ret.add(XMLDB.getAttr(n, "name"));
			} 
			if(n.getNodeName().equals(qg.yearLevel.getKey())) {
				if(qg.nodeLevel.getKey().equals("UndiscountedCost") || qg.nodeLevel.getKey().equals("DiscountedCost")) {
					if(isGlobal) {
						ret.add(0, "Global");
					} else {
						ret.add(0, XMLDB.getAttr(n, "name"));
					}
					/*
				} else if(qg.yearLevel.equals("DataPoint")) {
					// check the locks after this line, It might leave some
					ret.add(0, n.getFirstChild().getFirstChild().getNodeValue());
					*/
				} else {
					// check the locks after this line, It might leave some
					//ret.add(0, n.getFirstChild().getFirstChil!().getNodeValue());
					//ret.add(0, n.getFirstChild().getNodeValue());
					ret.add(0, n.getFirstChild().getNextSibling().getFirstChild().getNodeValue());
				}

				/*
				//ret.add(n.getAttributes().getNamedItem("name").getNodeValue());
				if(!getOneAttrVal(n).equals("fillout=1")) {
				ret.add(getOneAttrVal(n));
				} else {
				ret.add(getOneAttrVal(n, 1));
				}
				*/

			} else if(XMLDB.hasAttr(n) && !n.getNodeName().equals(qg.nodeLevel.getKey())) {
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
		DbViewer.xmlDB.printLockStats("CostCurveQueryBuilder.getRegionAndYearFromNode");
		return ret.toArray();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		/*
		if( (isGlobal && currNode.getNodeName().equals(qg.nodeLevel)) ) {
			currNode.delete();
			return tempMap;
		}
		*/
		if(XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(qg.nodeLevel.getKey()) 
				&& !currNode.getNodeName().equals(qg.yearLevel.getKey())) {
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
