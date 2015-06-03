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

public class GDPQueryBuilder extends QueryBuilder {
	public static Map<String, Boolean> varList;
	public static String xmlName = "gdpQueryBuilder";
	public GDPQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
	}
	public String createListPath(int level) {
		System.out.println("This Method doesn't do anything");
		return null;
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		return updateList(list, label);
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
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
			/*
			case 3: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(sectorList == null) {
						sectorList = createList("supplysector/@name", false);
						sectorList.putAll(createList("supplysector/group/@name", true));
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
			*/
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
			/*
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
			*/
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
		qg.xPath = "GDP/"+typeSel+"/text()";
		qg.nodeLevel = new DataPair<String, String>("region", "name");
		qg.yearLevel = new DataPair<String, String>(typeSel, "year");
		qg.axis1Name = "Region";
		qg.axis2Name = "Year";
		qg.var = typeSel;
		qg.sumAll = false;
		qg.group = false;
	}
	public String getCompleteXPath(Object[] regions) {
		System.out.println("Trying to complete xpath");
		StringBuffer strBuff = new StringBuffer();
		strBuff.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1));
		if(((String)regions[0]).equals("Global")) {
			strBuff.append("]/").append(qg.xPath);
			return strBuff.toString();
		} else {
			strBuff.append(" and (");
			for(int i = 0; i < regions.length; ++i) {
				strBuff.append(" (@name='").append(regions[i]).append("') or ");
			}
			strBuff.replace(strBuff.length()-3, strBuff.length(), ")]/");
			strBuff.append(qg.xPath);
			return strBuff.toString();
		}
	}
	public String getXMLName() {
		return xmlName;
	}
	public List<String> getDefaultCollpaseList() {
		return new Vector<String>();
	}
	public Map addToDataTree(ANode currNode, Map dataTree, DataPair<String, String> axisValue, boolean isGlobal) throws Exception {
        BXNode currDOM = new BXElem(currNode);
		// stop point for recursion is the root
		if (currDOM.getNodeType() == BXNode.DOCUMENT_NODE) {
			return dataTree;
		}

		// recursively process parents first
		Map tempMap = addToDataTree(currNode.parent(), dataTree, axisValue, isGlobal);

		// cache node properties
		final String nodeName = currDOM.getNodeName();
		final Map<String, String> attrMap = XMLDB.getAttrMap(currDOM);

		// attempt to find the axis at this node
		String type = attrMap.get("type");
		boolean addedNodeLevel = false;
		boolean addedYearLevel = false;
		if(qg.nodeLevel.getKey().equals(type)) {
			addedNodeLevel = true;
			if(qg.nodeLevel.getKey().equals("region") && isGlobal) {
				axisValue.setValue("Global");
			} else {
				axisValue.setValue(attrMap.get(qg.nodeLevel.getValue() != null ? qg.nodeLevel.getValue() : "name"));
			}
		}
		if(nodeName.equals(qg.yearLevel.getKey())) {
			addedYearLevel = true;
			axisValue.setKey(attrMap.get(qg.yearLevel.getValue() != null ? qg.yearLevel.getValue() : "year"));
		}

		// split maps if we shouldn't collapse at this node
		if(type == null) {
			type = nodeName;
		}
		// special case since male and female do not actually have attributes
		if(!addedNodeLevel && !addedYearLevel && !attrMap.isEmpty()) {
            // TODO: add ability to use showAttrMap
			String attr = XMLDB.getAllAttr(attrMap, null);
			// check for rewrites
			if(qg.labelRewriteMap != null && qg.labelRewriteMap.containsKey(type)) {
				Map<String, String> currRewriteMap = qg.labelRewriteMap.get(type);
				if(currRewriteMap.containsKey(attr)) {
					attr = currRewriteMap.get(attr);
				}
			}
			attr = type+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
			}
			tempMap = (Map)tempMap.get(attr);
		}
		return tempMap;
	}
}
