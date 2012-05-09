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
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class SAMQueryBuilder extends QueryBuilder {
	/*
	public static Map varList;
	protected Map sectorList;
	protected Map subsectorList;
	protected Map techList;
	protected Map inputList;
	*/
	public static String xmlName = "samQuery";
	public SAMQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
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
		// doing this stuff after currSel has changed now..
		// have to sub 1
		/*
		if(qg.currSel == 2) {
			sectorList = null;
		} else if(qg.currSel == 3) {
			subsectorList = null;
		} else if(qg.currSel == 4) {
			techList = null;
		} else if(qg.currSel == 5) {
			inputList = null;
		}
		*/
		return updateList(list, label);
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		// being moved to after currSel changed, adjust numbers
		updateSelected(list);
		if(qg.currSel == 3) {
			qg.var = "demand-currency";
			qg.isSumable = false;
		}
		return updateList(list, label);
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		/*
		Map temp = new HashMap();
		temp.put("Select This!!", false);
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					label.setText("Select Stuff:");
					break;
			}
			*/
			/*
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
			case 6: {
					if(inputList == null) {
						inputList = createList(createListPath(6), false);
					}
					temp = inputList;
					label.setText("Select Input:");
					break;
			}
			*/
		/*
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
		*/
		return list;
	}
	public void updateSelected(JComponentAdapter list) {
		/*
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
			case 6: {
					selected = inputList;
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
		*/
	}
	public String createListPath(int level) {
		/*
		Map tempMap;
		StringBuffer ret = new StringBuffer();
		boolean added = false;
		boolean gq = false;
		if(level == -1) {
			gq = true;
			qg.sumAll = qg.group = false;
			level = 7;
		}
		for(int i = 0; i < level-3; ++i) {
			if(i == 0) {
				tempMap = sectorList;
				ret.append(sectorQueryPortion.substring(0, sectorQueryPortion.length()-1));
			} else if(i == 1){
				tempMap = subsectorList;
				ret.append(subsectorQueryPortion.substring(0, subsectorQueryPortion.length()-1));
			} else if(i == 2){
				tempMap = techList;
				ret.append(technologyQueryPortion.substring(0, technologyQueryPortion.length()-1));
				//++i;
			} else {
				tempMap = inputList;
				ret.append(inputQueryPortion.substring(0, inputQueryPortion.length()-1));
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
		} else if(level == 6) {
			ret.append(inputQueryPortion+"/@name");
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
		*/
		// do I need to do anything real?
		return qg.xPath;
	}
	/*
	private String expandGroupName(String gName) {
		String query;
		StringBuffer ret = new StringBuffer();
		if(qg.currSel == 3) {
			//query = "supplysector";
			query = sectorQueryPortion;
		} else if(qg.currSel == 4) {
			query = sectorQueryPortion+"/"+subsectorQueryPortion;
		} else if(qg.currSel == 5){
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion;
		} else {
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion+
				"/"+inputQueryPortion;
		}
		XmlResults res = XMLDB.getInstance().createQuery(query+"[child::group[@name='"+gName+"']]/@name", queryFilter, queryFunctions);
		try {
			while(res.hasNext()) {
				ret.append("(@name='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		XMLDB.getInstance().printLockStats("SAMQueryBuilder.expandGroupName");
		return ret.toString();
	}
	*/
	private void createXPath() {
		qg.axis1Name = "expenditure";
		qg.nodeLevel = new DataPair<String, String>("expenditure", "name");
		qg.axis2Name = "sector";
		qg.yearLevel = new DataPair<String, String>("sector", "name");
		qg.group = true;
		qg.var = "*";
		/*
		qg.xPath = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+baseTechnologyQueryPortion+"/"
			+"expenditure/*[exists(index-of(('consumption', 'transfers', 'direct-taxes', 'savings', 'subsidy',"+
		        "'total-imports', 'investment', 'wages', 'landRents', 'rentals', 'indirect-taxes', 'intermediate-input',"+
		        "'sales', 'tariffs', 'imports', 'dividends', 'retained-earnings'), local-name()))]/text() | "+
			"Marketplace/market[child::MarketGoodOrFuel"+
			"[ child::text() = 'Land' or child::text() = 'Labor'  or child::text() = 'Capital']]/supply/text()";
			*/
		qg.xPath = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+baseTechnologyQueryPortion+"/"
			+"expenditure/*[(local-name() = 'total-imports' and parent::*/parent::*/@name = 'Trade') or "
			+"(local-name() = 'investment' and parent::*/parent::*/@name = 'Investment') or "
			+"(exists(index-of(('consumption', 'transfers', 'direct-taxes', 'savings'), local-name())) and "
			+"parent::*/parent::*/@name = 'Households') or (exists(index-of(('subsidy', 'consumption', "
			+"'transfers', 'savings'), local-name())) and parent::*/parent::*/@name = 'Government') or "
			+"(not(exists(index-of(('Trade', 'Investment', 'Households', 'Government'), parent::*/parent::*/@name))) "
			+"and exists(index-of(('intermediate-input', 'wages', 'land-rents', 'rentals', 'indirect-taxes', 'sales', "
			+"'tariffs', 'imports', 'dividends', 'direct-taxes', 'retained-earnings'), local-name())))]/text() | "
			+"Marketplace/market[child::MarketGoodOrFuel"
			+"[ child::text() = 'Land' or child::text() = 'Labor'  or child::text() = 'Capital']]/supply/text()";
	}
	/*
	private Map createList(String path, boolean isGroupNames) {
		LinkedHashMap ret = new LinkedHashMap();
		if(!isGroupNames && qg.isSumable) {
			ret.put("Sum All", new Boolean(false));
			ret.put("Group All", new Boolean(false));
		}
		XmlResults res = XMLDB.getInstance().createQuery(path, queryFilter, queryFunctions);
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
		XMLDB.getInstance().printLockStats("SAMQueryBuilder.createList");
		return ret;
	}
	*/
	public String getCompleteXPath(Object[] regions) {
		boolean added = false;
		StringBuilder ret = new StringBuilder();
		StringBuffer marketRegionQ = new StringBuffer();
        boolean isGlobal;
		if(((String)regions[0]).equals("Global")) {
			ret.append(regionQueryPortion+"/");
			isGlobal = true;
		} else {
			isGlobal = false;
		}
		for(int i = 0; i < regions.length && !isGlobal; ++i) {
			if(!added) {
				ret.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1)).append(" and (");
				marketRegionQ.append(" and ContainedRegion[");
				added = true;
			} else {
				ret.append(" or ");
				marketRegionQ.append(" or ");
			}
			ret.append("(@name='").append(regions[i]).append("')");
			marketRegionQ.append("(child::text() = '").append(regions[i]).append("')");
		}
		String[] queries = qg.getXPath().split("\\s*\\|\\s*");
		if(added) {
			ret.append(" )]/");
			marketRegionQ.append(" ] ]/");
			String[] spStr = queries[1].split("\\]/");
			marketRegionQ.insert(0, spStr[0]).append(spStr[1]).toString();
		} else {
			marketRegionQ.append(queries[1]);
		}
		return ret.append(queries[0]).append(" | ").append(marketRegionQ).toString();
		/*
		int pipeIndex = qg.getXPath().indexOf('|');
		String part1 = qg.getXPath().substring(0, pipeIndex+1);
		String part2 = qg.getXPath().substring(pipeIndex+1, qg.getXPath().length());
		String retStr = ret.toString();
		*/
		//System.out.println("XPath would be: "+ret.append(part1).append(retStr).append(part2));
		//return ret.append(qg.getXPath()).toString();
		/*
		return ret.append(part1)
			.append(" collection('database.dbxml')/scenario[ (@name='test' and @date='2006-27-9T22:20:46-00:00') ]/world/")
			.append(retStr).append(part2).toString();
			*/
	}
	public String getXMLName() {
		return xmlName;
	}
	public List<String> getDefaultCollpaseList() {
		return new Vector<String>();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree, DataPair<String, String> axisValue, boolean isGlobal) throws Exception {
		throw new UnsupportedOperationException("Have not converted this query builder yet");
	}
}
