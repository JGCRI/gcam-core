/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
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
package ModelInterface.ModelGUI2.queries;

import ModelInterface.ModelGUI2.xmldb.XMLDB;
import ModelInterface.common.DataPair;

import javax.swing.JList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.ListSelectionModel;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.EventListener;

import org.basex.query.value.node.ANode;
import org.basex.api.dom.BXNode;
import org.basex.api.dom.BXElem;

public class CostCurveQueryBuilder extends QueryBuilder {
	public static Map varList;
	public static String xmlName = "costCurveQuery";
    private static final List<String> showAttrList = new Vector<String>(2);
    static {
        showAttrList.add("year");
        showAttrList.add("type");
    }
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
			qg.axis2Name = "DiscountedCost";
			qg.yearLevel = new DataPair<String, String>("DiscountedCost", null);
			qg.nodeLevel = new DataPair<String, String>("DiscountedCost", null);
			qg.axis1Name = "Region";
			qg.var = "Discounted Cost";
		} else if(typeSel.equals("RegionalUndiscountedCosts")) {
			qg.xPath = "/text()";
			qg.axis2Name = "UndiscountedCost";
			qg.yearLevel = new DataPair<String, String>("UndiscountedCost", null);
			qg.nodeLevel = new DataPair<String, String>("UndiscountedCost", null);
			qg.axis1Name = "Region";
			qg.var = "Undiscounted Cost";
		} else {
			qg.nodeLevel = new DataPair<String, String>("Curve", null);
			qg.yearLevel = new DataPair<String, String>("DataPoint", null);
			qg.axis1Name = "Region";
			qg.var = "Cost";
			if(typeSel.equals("RegionalCostCurvesByPeriod")) {
                qg.xPath = "PointSet/DataPoint/y/text()";
				qg.axis2Name = "Year";
			} else {
                qg.xPath = "PointSet/DataPoint/x/text()";
				qg.axis2Name = "Iteration";
			}
		}
		qg.group = false;
		qg.sumAll = false;
	}
	public String getCompleteXPath(Object[] regions) {
		System.out.println("Trying to complete xpath");
		StringBuffer strBuff = new StringBuffer();
		if(qg.nodeLevel.getKey().equals("DiscountedCost") || qg.nodeLevel.getKey().equals("UndiscountedCost")) {
			strBuff.append("CostCurvesInfo/").append("Regional").append(qg.nodeLevel.getKey())
				.append("s/").append(qg.nodeLevel.getKey());
			if(((String)regions[0]).equals("Global")) {
				strBuff.append("/").append(qg.xPath);
				return strBuff.toString();
			} else {
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
				return strBuff.toString();
			} else {
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
				return strBuff.toString();
			} else {
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
	public String getXMLName() {
		return xmlName;
	}
	public List<String> getDefaultCollpaseList() {
		Vector<String> ret = new Vector<String>(1);
		ret.add("PointSet");
		return ret;
	}
	public Map addToDataTree(ANode currNode, Map dataTree, DataPair<String, String> axisValue, boolean isGlobal) throws Exception {
        BXNode currDOM = new BXElem(currNode);
		// stop condition for recursion when we hit the root of the tree
		if(currDOM.getNodeType() == BXNode.DOCUMENT_NODE) {
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.parent(), dataTree, axisValue, isGlobal);

		// cache node properties
		final String nodeName = currDOM.getNodeName();
		final Map<String, String> attrMap = XMLDB.getAttrMap(currDOM);
		boolean addedNodeLevel = false;
		boolean addedYearLevel = false;

		if(nodeName.equals(qg.nodeLevel.getKey())) {
			addedNodeLevel = true;
			if(qg.nodeLevel.getKey().equals("UndiscountedCost") || qg.nodeLevel.getKey().equals("DiscountedCost")) {
				// check this is was adding to pos 0 but I think it should be nodeLevel
				//ret.add(0, qg.nodeLevel.getKey());
				if(isGlobal) {
					// TODO: check if it should be key
					axisValue.setValue("Global");
				} else {
					// TODO: check if it should be key
					axisValue.setValue(attrMap.get("name"));
				}
			} else if(isGlobal) {
				axisValue.setValue("Global");
			} else {
				axisValue.setValue(currDOM.getFirstChild().getFirstChild().getNodeValue());
			}

		} 
		if(nodeName.equals(qg.yearLevel.getKey())) {
			addedYearLevel = true;
			if(qg.nodeLevel.getKey().equals("UndiscountedCost") || qg.nodeLevel.getKey().equals("DiscountedCost")) {
				axisValue.setKey(qg.nodeLevel.getKey());
		} else {
            if(qg.axis2Name.equals("Year")) {
                axisValue.setKey(currDOM.getFirstChild().getFirstChild().getNodeValue());
            } else {
                axisValue.setKey(currDOM.getFirstChild().getNextSibling().getFirstChild().getNodeValue());
            }
		}

		} 


		// check if we need to collapse this level
		if(!addedNodeLevel && !addedYearLevel && !attrMap.isEmpty()) {
            // TODO: add the ability to use showAttrMap
			String attr = XMLDB.getAllAttr(attrMap, showAttrList);
			attr = nodeName+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
			}
			tempMap = (Map)tempMap.get(attr);
		} 
		return tempMap;
	}
}
